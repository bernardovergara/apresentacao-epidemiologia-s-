if (!requireNamespace("dplyr", quietly = TRUE)) {
	install.packages("dplyr")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
	install.packages("ggplot2")
}

if (!requireNamespace("read.dbc", quietly = TRUE)) {
	install.packages("read.dbc")
}

if (!requireNamespace("tidyr", quietly = TRUE)) {
	install.packages("tidyr")
}

library(dplyr)
library(ggplot2)
library(tidyr)

if (!requireNamespace("scales", quietly = TRUE)) {
	install.packages("scales")
}

formatar_numero <- scales::label_number(
	big.mark = ".",
	decimal.mark = ",",
	accuracy = 1
)

options(scipen = 999)

# Os arquivos de violencia do SINAN sao baixados como DBC e lidos pelo
# pacote read.dbc. Coloque os arquivos na pasta abaixo.
pasta_dbc <- file.path("dados", "brutos")
arquivos_dbc <- list.files(pasta_dbc, pattern = "\\.dbc$", full.names = TRUE,
	                           ignore.case = TRUE)

cache_dir <- file.path(pasta_dbc, "cache")
if (!dir.exists(cache_dir)) {
	dir.create(cache_dir, recursive = TRUE)
}

if (length(arquivos_dbc) == 0) {
	stop(
		"Nenhum arquivo DBC encontrado em '", pasta_dbc,
		"'. Baixe os arquivos de violencia do SINAN e coloque-os nessa pasta."
	)
}

message("Encontrados ", length(arquivos_dbc), " arquivos DBC.")

lista_dados <- vector("list", length(arquivos_dbc))

for (i in seq_along(arquivos_dbc)) {
	arquivo <- arquivos_dbc[i]
	arquivo_cache <- file.path(cache_dir, paste0(basename(arquivo), ".rds"))
	inicio <- Sys.time()
	cache_valido <- file.exists(arquivo_cache) &&
		isTRUE(file.info(arquivo_cache)$mtime >= file.info(arquivo)$mtime)

	if (cache_valido) {
		message("[", i, "/", length(arquivos_dbc), "] Usando cache de ",
			basename(arquivo), "...")
		lista_dados[[i]] <- readRDS(arquivo_cache)
	} else {
		message("[", i, "/", length(arquivos_dbc), "] Lendo ", basename(arquivo), "...")
		lista_dados[[i]] <- read.dbc::read.dbc(arquivo)
		ano_arquivo <- as.integer(sub("^.*VIOLBR([0-9]{2})\\.dbc$", "\\1",
			basename(arquivo), ignore.case = TRUE))
		lista_dados[[i]]$ano_arquivo <- 2000 + ano_arquivo
		saveRDS(lista_dados[[i]], arquivo_cache)
	}

	tempo <- round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 1)
	message(
		"[", i, "/", length(arquivos_dbc), "] Concluido: ",
		nrow(lista_dados[[i]]), " registros em ", tempo, " s."
	)
}

message("Combinando os arquivos...")
dados <- dplyr::bind_rows(lista_dados)
message("Total de registros: ", nrow(dados))

colunas_necessarias <- c("LES_AUTOP")
colunas_faltantes <- setdiff(colunas_necessarias, names(dados))

if (length(colunas_faltantes) > 0) {
	stop(
		"A coluna LES_AUTOP nao foi encontrada. Colunas disponiveis: ",
		paste(names(dados), collapse = ", ")
	)
}

message("Filtrando apenas suicidio e tentativas (LES_AUTOP = 1)...")
dados_violencia <- dados %>%
	mutate(
		ano = ano_arquivo,
		suicidio_ou_tentativa = toupper(trimws(as.character(LES_AUTOP))) %in%
			c("1", "SIM", "S")
	) %>%
	filter(!is.na(ano), suicidio_ou_tentativa)

message("Registros apos o filtro: ", nrow(dados_violencia))
saveRDS(dados_violencia, file.path(cache_dir, "dados_violencia.rds"))

graficos_dir <- file.path("resultados", "graficos")
dir.create(graficos_dir, recursive = TRUE, showWarnings = FALSE)
setwd(graficos_dir)

casos_por_ano <- dados_violencia %>%
	count(ano, name = "casos") %>%
	arrange(ano)

print(casos_por_ano)

message("Gerando grafico...")
grafico_total_notificacoes <- ggplot(casos_por_ano, aes(x = ano, y = casos)) +
	geom_line(linewidth = 0.8, color = "#B2182B") +
	geom_point(size = 2, color = "#B2182B") +
	scale_x_continuous(breaks = casos_por_ano$ano) +
	scale_y_continuous(labels = formatar_numero) +
	labs(
		title = "Notificacoes de suicidio e tentativas no SINAN",
		subtitle = "Brasil, por ano de notificacao",
		x = "Ano",
		y = "Numero de notificacoes",
		caption = "Fonte: SINAN / DATASUS via microdatasus"
	) +
	theme_minimal()

print(grafico_total_notificacoes)
ggsave("grafico_total_notificacoes_por_ano.png", grafico_total_notificacoes,
	width = 8, height = 5, dpi = 300)
message("Grafico salvo em: ", normalizePath("grafico_total_notificacoes_por_ano.png"))

desfechos_evolucao <- c(
	"Tentativa com sucesso",
	"Tentativa sem sucesso",
	"Sem informacao"
)

dados_evolucao <- dados_violencia %>%
	mutate(
		codigo_evolucao = suppressWarnings(
			as.integer(sub("^\\s*([0-9]+).*", "\\1", as.character(EVOLUCAO)))
		),
		desfecho = factor(
			case_when(
				codigo_evolucao == 1 ~ "Tentativa sem sucesso",
				codigo_evolucao == 2 ~ "Tentativa com sucesso",
				TRUE ~ "Sem informacao"
			),
			levels = desfechos_evolucao
		)
	)

evolucao_por_ano <- dados_evolucao %>%
		count(ano, desfecho, name = "casos") %>%
		tidyr::complete(
			ano = sort(unique(dados_evolucao$ano)),
			desfecho = desfechos_evolucao,
			fill = list(casos = 0)
		) %>%
		arrange(ano, desfecho)

grafico_total_notificacoes_por_ano_evolucao <- ggplot(
	evolucao_por_ano,
	aes(x = ano, y = casos, color = desfecho, group = desfecho)
) +
	geom_line(linewidth = 0.8) +
	geom_point(size = 2) +
	scale_x_continuous(breaks = sort(unique(evolucao_por_ano$ano))) +
	scale_y_continuous(labels = formatar_numero) +
	labs(
		title = "Total de notificacoes por ano e evolucao",
		x = "Ano",
		y = "Numero de notificacoes",
		color = "Evolucao"
	) +
	theme_minimal()

print(grafico_total_notificacoes_por_ano_evolucao)
ggsave(
	"grafico_total_notificacoes_por_ano_evolucao.png",
	grafico_total_notificacoes_por_ano_evolucao,
	width = 10, height = 5, dpi = 300
)

evolucao_por_ano_informada <- evolucao_por_ano %>%
	filter(desfecho != "Sem informacao")

grafico_total_notificacoes_por_ano_evolucao_informada <- ggplot(
	evolucao_por_ano_informada,
	aes(x = ano, y = casos, color = desfecho, group = desfecho)
) +
	geom_line(linewidth = 0.8) +
	geom_point(size = 2) +
	scale_x_continuous(breaks = sort(unique(evolucao_por_ano_informada$ano))) +
	scale_y_continuous(labels = formatar_numero) +
	labs(
		title = "Total de notificacoes por ano e evolucao",
		subtitle = "Apenas casos com evolucao informada",
		x = "Ano",
		y = "Numero de notificacoes",
		color = "Evolucao"
	) +
	theme_minimal()

print(grafico_total_notificacoes_por_ano_evolucao_informada)
ggsave(
	"grafico_total_notificacoes_por_ano_evolucao_informada.png",
	grafico_total_notificacoes_por_ano_evolucao_informada,
	width = 10, height = 5, dpi = 300
)

# -----------------------------------------------------------------------------
# 1. Suicidio e tentativas por sexo, ao longo dos anos
# -----------------------------------------------------------------------------
dados_analise <- dados_violencia %>%
	mutate(
		sexo = case_when(
			CS_SEXO %in% c("M", "1") ~ "Masculino",
			CS_SEXO %in% c("F", "2") ~ "Feminino",
			TRUE ~ "Ignorado"
		),
		raca_cor = case_when(
		as.character(CS_RACA) %in% c("1", "Branca") ~ "Branca",
		as.character(CS_RACA) %in% c("2", "Preta") ~ "Preta",
		as.character(CS_RACA) %in% c("3", "Amarela") ~ "Amarela",
		as.character(CS_RACA) %in% c("4", "Parda") ~ "Parda",
		as.character(CS_RACA) %in% c("5", "Indigena") ~ "Indigena",
		TRUE ~ "Ignorado"
		)
	) %>%
	filter(sexo != "Ignorado")

sexo_por_ano <- dados_analise %>%
	count(ano, sexo, name = "casos") %>%
	arrange(ano, sexo)

print(sexo_por_ano)

grafico_sexo <- ggplot(sexo_por_ano, aes(x = ano, y = casos, color = sexo)) +
	geom_line(linewidth = 0.8) +
	geom_point(size = 2) +
	scale_x_continuous(breaks = sort(unique(sexo_por_ano$ano))) +
	scale_y_continuous(labels = formatar_numero) +
	labs(
		title = "Suicidio e tentativas por sexo",
		x = "Ano",
		y = "Numero de notificacoes",
		color = "Sexo"
	) +
	theme_minimal()

print(grafico_sexo)
ggsave("grafico_suicidio_por_sexo.png", grafico_sexo,
	width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 2. Suicidio e tentativas por raca/cor, ao longo dos anos
# -----------------------------------------------------------------------------
raca_por_ano <- dados_analise %>%
	filter(raca_cor != "Ignorado") %>%
	count(ano, raca_cor, name = "casos") %>%
	arrange(ano, raca_cor)

print(raca_por_ano)

grafico_raca <- ggplot(raca_por_ano, aes(x = ano, y = casos, color = raca_cor)) +
	geom_line(linewidth = 0.8) +
	geom_point(size = 2) +
	scale_x_continuous(breaks = sort(unique(raca_por_ano$ano))) +
	scale_y_continuous(labels = formatar_numero) +
	labs(
		title = "Suicidio e tentativas por raca/cor",
		x = "Ano",
		y = "Numero de notificacoes",
		color = "Raca/cor"
	) +
	theme_minimal()

print(grafico_raca)
ggsave("grafico_suicidio_por_raca_cor.png", grafico_raca,
	width = 8, height = 5, dpi = 300)

sexo_raca_por_ano <- dados_violencia %>%
	mutate(
		sexo = case_when(
			CS_SEXO %in% c("M", "1") ~ "Masculino",
			CS_SEXO %in% c("F", "2") ~ "Feminino",
			TRUE ~ "Ignorado"
		),
		raca_cor = case_when(
			as.character(CS_RACA) %in% c("1", "Branca") ~ "Branca",
			as.character(CS_RACA) %in% c("2", "Preta") ~ "Preta",
			as.character(CS_RACA) %in% c("3", "Amarela") ~ "Amarela",
			as.character(CS_RACA) %in% c("4", "Parda") ~ "Parda",
			as.character(CS_RACA) %in% c("5", "Indigena") ~ "Indigena",
			TRUE ~ "Ignorado"
		)
	) %>%
	filter(sexo != "Ignorado", raca_cor != "Ignorado") %>%
	count(ano, sexo, raca_cor, name = "casos") %>%
	arrange(ano, sexo, raca_cor)

grafico_homens_raca_cor <- ggplot(
	sexo_raca_por_ano %>% filter(sexo == "Masculino"),
	aes(x = ano, y = casos, color = raca_cor, group = raca_cor)
) +
	geom_line(linewidth = 0.8) +
	geom_point(size = 2) +
	scale_x_continuous(breaks = sort(unique(sexo_raca_por_ano$ano))) +
	scale_y_continuous(labels = formatar_numero) +
	labs(
		title = "Homens por raca/cor ao longo dos anos",
		x = "Ano",
		y = "Numero de notificacoes",
		color = "Raca/cor"
	) +
	theme_minimal()

print(grafico_homens_raca_cor)
ggsave("grafico_homens_por_raca_cor_ao_longo_dos_anos.png",
	grafico_homens_raca_cor, width = 10, height = 5, dpi = 300)

grafico_mulheres_raca_cor <- ggplot(
	sexo_raca_por_ano %>% filter(sexo == "Feminino"),
	aes(x = ano, y = casos, color = raca_cor, group = raca_cor)
) +
	geom_line(linewidth = 0.8) +
	geom_point(size = 2) +
	scale_x_continuous(breaks = sort(unique(sexo_raca_por_ano$ano))) +
	scale_y_continuous(labels = formatar_numero) +
	labs(
		title = "Mulheres por raca/cor ao longo dos anos",
		x = "Ano",
		y = "Numero de notificacoes",
		color = "Raca/cor"
	) +
	theme_minimal()

print(grafico_mulheres_raca_cor)
ggsave("grafico_mulheres_por_raca_cor_ao_longo_dos_anos.png",
	grafico_mulheres_raca_cor, width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 3. Distribuicao da idade em faixas de 5 anos, considerando todos os anos
# -----------------------------------------------------------------------------
# No SINAN, NU_IDADE_N usa 4xxx para idade em anos. O fallback permite
# trabalhar tambem com arquivos que ja tragam a idade sem essa codificacao.
idade_bruta <- suppressWarnings(as.integer(as.character(dados_analise$NU_IDADE_N)))
idade_anos <- dplyr::case_when(
	idade_bruta >= 4000 ~ idade_bruta %% 1000,
	idade_bruta >= 0 & idade_bruta <= 120 ~ idade_bruta,
	TRUE ~ NA_real_
)

dados_idade <- dados_analise %>%
	mutate(idade_anos = idade_anos) %>%
	filter(!is.na(idade_anos), idade_anos >= 0, idade_anos <= 120) %>%
	mutate(
		faixa_idade = cut(
			idade_anos,
			breaks = c(-1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50,
				55, 60, 65, 70, 75, 80, 85, Inf),
			labels = c("0 a 5", "6 a 10", "11 a 15", "16 a 20", "21 a 25",
				"26 a 30", "31 a 35", "36 a 40", "41 a 45", "46 a 50",
				"51 a 55", "56 a 60", "61 a 65", "66 a 70", "71 a 75",
				"76 a 80", "81 a 85", "86 ou mais")
		)
	)

idade_por_faixa <- dados_idade %>%
	count(faixa_idade, name = "casos")

print(idade_por_faixa)

grafico_idade <- ggplot(idade_por_faixa, aes(x = faixa_idade, y = casos, group = 1)) +
	geom_line(linewidth = 0.8, color = "#2166AC") +
	geom_point(size = 2, color = "#2166AC") +
	scale_y_continuous(labels = formatar_numero) +
	labs(
		title = "Distribuicao por faixa etaria",
		subtitle = "Todos os anos",
		x = "Faixa de idade (anos)",
		y = "Numero de notificacoes"
	) +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_idade)
ggsave("grafico_suicidio_por_idade.png", grafico_idade,
	width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 4. Distribuicao por escolaridade, considerando todos os anos
# -----------------------------------------------------------------------------
escolaridade_por_nome <- function(x) {
	codigo <- suppressWarnings(as.integer(as.character(x)))

	dplyr::recode(
		as.character(codigo),
		`0` = "Analfabeto",
		`1` = "1a a 4a serie incompleta do EF",
		`2` = "4a serie completa do EF",
		`3` = "5a a 8a serie incompleta do EF",
		`4` = "Ensino fundamental completo",
		`5` = "Ensino medio incompleto",
		`6` = "Ensino medio completo",
		`7` = "Educacao superior incompleta",
		`8` = "Educacao superior completa",
		`9` = "Ignorado",
		`10` = "Nao se aplica",
		.default = as.character(x)
	)
}

escolaridade_por_ano <- dados_analise %>%
	mutate(escolaridade = escolaridade_por_nome(CS_ESCOL_N)) %>%
	filter(!is.na(escolaridade)) %>%
	count(escolaridade, name = "casos") %>%
	arrange(desc(casos))

print(escolaridade_por_ano)

grafico_escolaridade <- ggplot(
	escolaridade_por_ano,
	aes(x = reorder(escolaridade, casos), y = casos)
) +
	geom_col(fill = "#4D9221") +
	coord_flip() +
	scale_y_continuous(labels = formatar_numero) +
	labs(
		title = "Distribuicao por escolaridade",
		subtitle = "Todos os anos, incluindo ignorado e nao se aplica",
		x = "Escolaridade",
		y = "Numero de notificacoes"
	) +
	theme_minimal()

print(grafico_escolaridade)
ggsave("grafico_suicidio_por_escolaridade.png", grafico_escolaridade,
	width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 5. Quantidade por idade: tentativa com sucesso x tentativa sem sucesso
# -----------------------------------------------------------------------------
# Convencao usada: EVOLUCAO = 1 indica cura/alta e EVOLUCAO = 2 indica obito
# por violencia. Valores ausentes ou desconhecidos entram como sem informacao.
if (!"EVOLUCAO" %in% names(dados_idade)) {
	stop(
		"A coluna EVOLUCAO nao foi encontrada. Ela e necessaria para separar ",
		"tentativa com sucesso de tentativa sem sucesso."
	)
}

desfechos <- c(
	"Tentativa sem sucesso",
	"Tentativa com sucesso",
	"Sem informacao"
)

dados_desfecho <- dados_idade %>%
	mutate(
		codigo_evolucao = suppressWarnings(
			as.integer(sub("^\\s*([0-9]+).*", "\\1", as.character(EVOLUCAO)))
		),
		desfecho = factor(
			case_when(
				codigo_evolucao == 1 ~ "Tentativa sem sucesso",
				codigo_evolucao == 2 ~ "Tentativa com sucesso",
				TRUE ~ "Sem informacao"
			),
			levels = desfechos
		)
	)


quantidade_por_idade <- dados_desfecho %>%
	count(faixa_idade, desfecho, name = "casos")

print(quantidade_por_idade)

grafico_idade_desfecho <- ggplot(
	quantidade_por_idade,
	aes(x = faixa_idade, y = casos, fill = desfecho)
) +
	geom_col(position = "dodge") +
	scale_y_continuous(labels = formatar_numero) +
	labs(
		title = "Quantidade de tentativas por faixa etaria",
		subtitle = "Todo o periodo analisado",
		x = "Faixa etaria (anos)",
		y = "Quantidade de notificacoes",
		fill = "Desfecho"
	) +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_idade_desfecho)
ggsave("grafico_quantidade_idade_por_desfecho.png", grafico_idade_desfecho,
	width = 10, height = 5, dpi = 300)

quantidade_por_idade_com_desfecho <- quantidade_por_idade %>%
	filter(desfecho != "Sem informacao")

grafico_idade_desfecho_apenas_classificados <- ggplot(
	quantidade_por_idade_com_desfecho,
	aes(x = faixa_idade, y = casos, fill = desfecho)
) +
	geom_col(position = "dodge") +
	scale_y_continuous(labels = formatar_numero) +
	labs(
		title = "Quantidade de tentativas por faixa etaria",
		subtitle = "Apenas casos com desfecho informado",
		x = "Faixa etaria (anos)",
		y = "Quantidade de notificacoes",
		fill = "Desfecho"
	) +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_idade_desfecho_apenas_classificados)
ggsave("grafico_quantidade_idade_por_desfecho_informado.png",
	grafico_idade_desfecho_apenas_classificados,
	width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 6. Quantidade mensal de tentativas e sucessos em todos os anos
# -----------------------------------------------------------------------------
dados_meses <- dados_violencia %>%
	mutate(
		data_notificacao = as.Date(
			if (inherits(DT_NOTIFIC, "Date")) {
				DT_NOTIFIC
			} else {
				valor_data <- as.character(DT_NOTIFIC)
				as.Date(
					ifelse(
						grepl("^[0-9]{8}$", valor_data),
						paste0(substr(valor_data, 1, 4), "-",
							substr(valor_data, 5, 6), "-",
							substr(valor_data, 7, 8)),
						valor_data
					),
					tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%Y%m%d")
				)
			}
		),
		mes = as.integer(format(data_notificacao, "%m")),
		codigo_evolucao = suppressWarnings(
			as.integer(sub("^\\s*([0-9]+).*", "\\1", as.character(EVOLUCAO)))
		),
		desfecho = factor(
			case_when(
				codigo_evolucao == 1 ~ "Tentativa sem sucesso",
				codigo_evolucao == 2 ~ "Tentativa com sucesso",
				TRUE ~ "Sem informacao"
			),
			levels = desfechos
		)
	) %>%
	filter(!is.na(mes), !is.na(ano))

meses <- data.frame(
	mes = 1:12,
	nome_mes = factor(
		c("Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho",
			"Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"),
		levels = c("Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho",
			"Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
	)
)

quantidade_mensal <- dados_meses %>%
	count(mes, desfecho, name = "casos") %>%
	tidyr::complete(mes = 1:12, desfecho = desfechos, fill = list(casos = 0)) %>%
	left_join(meses, by = "mes") %>%
	arrange(mes, desfecho)

print(quantidade_mensal)

grafico_mensal <- ggplot(
	quantidade_mensal,
	aes(x = nome_mes, y = casos, color = desfecho, group = desfecho)
) +
	geom_line(linewidth = 0.8) +
	geom_point(size = 2) +
	scale_y_continuous(labels = formatar_numero) +
	labs(
		title = "Quantidade mensal de tentativas e sucessos",
		subtitle = "Todos os anos analisados",
		x = "Mes",
		y = "Quantidade de notificacoes",
		color = "Desfecho"
	) +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_mensal)
ggsave("grafico_quantidade_mensal.png", grafico_mensal,
	width = 10, height = 5, dpi = 300)

quantidade_mensal_com_desfecho <- quantidade_mensal %>%
	filter(desfecho != "Sem informacao")

grafico_mensal_apenas_classificados <- ggplot(
	quantidade_mensal_com_desfecho,
	aes(x = nome_mes, y = casos, color = desfecho, group = desfecho)
) +
	geom_line(linewidth = 0.8) +
	geom_point(size = 2) +
	scale_y_continuous(labels = formatar_numero) +
	labs(
		title = "Quantidade mensal de tentativas e sucessos",
		subtitle = "Apenas casos com desfecho informado",
		x = "Mes",
		y = "Quantidade de notificacoes",
		color = "Desfecho"
	) +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_mensal_apenas_classificados)
ggsave("grafico_quantidade_mensal_desfecho_informado.png",
	grafico_mensal_apenas_classificados,
	width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 7. Graficos separados para 2024 e 2025
# -----------------------------------------------------------------------------
gerar_graficos_ano <- function(ano_filtro) {
	dados_ano <- dados_violencia %>%
		filter(ano == ano_filtro)

	if (nrow(dados_ano) == 0) {
		warning("Nenhum registro encontrado para ", ano_filtro, ".")
		return(invisible(NULL))
	}

	sufixo <- as.character(ano_filtro)

	sexo_ano <- dados_ano %>%
		mutate(
			sexo = case_when(
				CS_SEXO %in% c("M", "1") ~ "Masculino",
				CS_SEXO %in% c("F", "2") ~ "Feminino",
				TRUE ~ "Ignorado"
			)
		) %>%
		filter(sexo != "Ignorado") %>%
		count(sexo, name = "casos")

	grafico_sexo_ano <- ggplot(sexo_ano, aes(x = sexo, y = casos, fill = sexo)) +
		geom_col(show.legend = FALSE) +
		scale_y_continuous(labels = formatar_numero) +
		labs(
			title = paste("Suicidio e tentativas por sexo -", sufixo),
			x = "Sexo",
			y = "Numero de notificacoes"
		) +
		theme_minimal()

	ggsave(paste0("grafico_suicidio_por_sexo_", sufixo, ".png"),
		grafico_sexo_ano, width = 8, height = 5, dpi = 300)

	raca_ano <- dados_ano %>%
		mutate(
			raca_cor = case_when(
				as.character(CS_RACA) %in% c("1", "Branca") ~ "Branca",
				as.character(CS_RACA) %in% c("2", "Preta") ~ "Preta",
				as.character(CS_RACA) %in% c("3", "Amarela") ~ "Amarela",
				as.character(CS_RACA) %in% c("4", "Parda") ~ "Parda",
				as.character(CS_RACA) %in% c("5", "Indigena") ~ "Indigena",
				TRUE ~ "Ignorado"
			)
		) %>%
		filter(raca_cor != "Ignorado") %>%
		count(raca_cor, name = "casos")

	grafico_raca_ano <- ggplot(raca_ano, aes(x = raca_cor, y = casos, fill = raca_cor)) +
		geom_col(show.legend = FALSE) +
		scale_y_continuous(labels = formatar_numero) +
		labs(
			title = paste("Suicidio e tentativas por raca/cor -", sufixo),
			x = "Raca/cor",
			y = "Numero de notificacoes"
		) +
		theme_minimal()

	ggsave(paste0("grafico_suicidio_por_raca_cor_", sufixo, ".png"),
		grafico_raca_ano, width = 8, height = 5, dpi = 300)

	idade_ano <- dados_ano %>%
		mutate(
			idade_bruta = suppressWarnings(as.integer(as.character(NU_IDADE_N))),
			idade_anos = case_when(
				idade_bruta >= 4000 ~ idade_bruta %% 1000,
				idade_bruta >= 0 & idade_bruta <= 120 ~ idade_bruta,
				TRUE ~ NA_real_
			),
			faixa_idade = cut(
				idade_anos,
				breaks = c(-1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50,
					55, 60, 65, 70, 75, 80, 85, Inf),
				labels = c("0 a 5", "6 a 10", "11 a 15", "16 a 20", "21 a 25",
					"26 a 30", "31 a 35", "36 a 40", "41 a 45", "46 a 50",
					"51 a 55", "56 a 60", "61 a 65", "66 a 70", "71 a 75",
					"76 a 80", "81 a 85", "86 ou mais")
			)
		) %>%
		filter(!is.na(faixa_idade)) %>%
		count(faixa_idade, name = "casos")

	grafico_idade_ano <- ggplot(idade_ano, aes(x = faixa_idade, y = casos, group = 1)) +
		geom_line(linewidth = 0.8, color = "#2166AC") +
		geom_point(size = 2, color = "#2166AC") +
		scale_y_continuous(labels = formatar_numero) +
		labs(
			title = paste("Distribuicao por faixa etaria -", sufixo),
			x = "Faixa de idade (anos)",
			y = "Numero de notificacoes"
		) +
		theme_minimal() +
		theme(axis.text.x = element_text(angle = 45, hjust = 1))

	ggsave(paste0("grafico_suicidio_por_idade_", sufixo, ".png"),
		grafico_idade_ano, width = 10, height = 5, dpi = 300)

	escolaridade_ano <- dados_ano %>%
		mutate(escolaridade = escolaridade_por_nome(CS_ESCOL_N)) %>%
		filter(!is.na(escolaridade)) %>%
		count(escolaridade, name = "casos") %>%
		arrange(desc(casos))

	grafico_escolaridade_ano <- ggplot(
		escolaridade_ano,
		aes(x = reorder(escolaridade, casos), y = casos)
	) +
		geom_col(fill = "#4D9221") +
		coord_flip() +
		scale_y_continuous(labels = formatar_numero) +
		labs(
			title = paste("Distribuicao por escolaridade -", sufixo),
			x = "Escolaridade",
			y = "Numero de notificacoes"
		) +
		theme_minimal()

	ggsave(paste0("grafico_suicidio_por_escolaridade_", sufixo, ".png"),
		grafico_escolaridade_ano, width = 8, height = 5, dpi = 300)

	desfecho_ano <- dados_ano %>%
		mutate(
			idade_bruta = suppressWarnings(as.integer(as.character(NU_IDADE_N))),
			idade_anos = case_when(
				idade_bruta >= 4000 ~ idade_bruta %% 1000,
				idade_bruta >= 0 & idade_bruta <= 120 ~ idade_bruta,
				TRUE ~ NA_real_
			),
			faixa_idade = cut(
				idade_anos,
				breaks = c(-1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50,
					55, 60, 65, 70, 75, 80, 85, Inf),
				labels = c("0 a 5", "6 a 10", "11 a 15", "16 a 20", "21 a 25",
					"26 a 30", "31 a 35", "36 a 40", "41 a 45", "46 a 50",
					"51 a 55", "56 a 60", "61 a 65", "66 a 70", "71 a 75",
					"76 a 80", "81 a 85", "86 ou mais")
			),
			codigo_evolucao = suppressWarnings(
				as.integer(sub("^\\s*([0-9]+).*", "\\1", as.character(EVOLUCAO)))
			),
			desfecho = case_when(
				codigo_evolucao == 1 ~ "Tentativa sem sucesso",
				codigo_evolucao == 2 ~ "Tentativa com sucesso",
				TRUE ~ "Sem informacao"
			)
		) %>%
		filter(!is.na(faixa_idade)) %>%
		count(faixa_idade, desfecho, name = "casos")

	grafico_desfecho_ano <- ggplot(
		desfecho_ano,
		aes(x = faixa_idade, y = casos, fill = desfecho)
	) +
		geom_col(position = "dodge") +
		scale_y_continuous(labels = formatar_numero) +
		labs(
			title = paste("Quantidade por faixa etaria e desfecho -", sufixo),
			x = "Faixa etaria (anos)",
			y = "Quantidade de notificacoes",
			fill = "Desfecho"
		) +
		theme_minimal() +
		theme(axis.text.x = element_text(angle = 45, hjust = 1))

	ggsave(paste0("grafico_quantidade_idade_por_desfecho_", sufixo, ".png"),
		grafico_desfecho_ano, width = 10, height = 5, dpi = 300)

	desfecho_ano_informado <- desfecho_ano %>%
		filter(desfecho != "Sem informacao")

	grafico_desfecho_ano_informado <- ggplot(
		desfecho_ano_informado,
		aes(x = faixa_idade, y = casos, fill = desfecho)
	) +
		geom_col(position = "dodge") +
		scale_y_continuous(labels = formatar_numero) +
		labs(
			title = paste("Quantidade por faixa etaria e desfecho -", sufixo),
			subtitle = "Apenas casos com desfecho informado",
			x = "Faixa etaria (anos)",
			y = "Quantidade de notificacoes",
			fill = "Desfecho"
		) +
		theme_minimal() +
		theme(axis.text.x = element_text(angle = 45, hjust = 1))

	ggsave(
		paste0("grafico_quantidade_idade_por_desfecho_informado_", sufixo, ".png"),
		grafico_desfecho_ano_informado,
		width = 10, height = 5, dpi = 300
	)

	mensal_ano <- dados_ano %>%
		mutate(
			data_notificacao = as.Date(
				if (inherits(DT_NOTIFIC, "Date")) {
					DT_NOTIFIC
				} else {
					valor_data <- as.character(DT_NOTIFIC)
					as.Date(
						ifelse(
							grepl("^[0-9]{8}$", valor_data),
							paste0(substr(valor_data, 1, 4), "-",
								substr(valor_data, 5, 6), "-",
								substr(valor_data, 7, 8)),
							valor_data
						),
						tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%Y%m%d")
					)
				}
			),
			mes = as.integer(format(data_notificacao, "%m")),
			codigo_evolucao = suppressWarnings(
				as.integer(sub("^\\s*([0-9]+).*", "\\1", as.character(EVOLUCAO)))
			),
			desfecho = case_when(
				codigo_evolucao == 1 ~ "Tentativa sem sucesso",
				codigo_evolucao == 2 ~ "Tentativa com sucesso",
				TRUE ~ "Sem informacao"
			)
		) %>%
		filter(!is.na(mes)) %>%
		count(mes, desfecho, name = "casos") %>%
		tidyr::complete(mes = 1:12, desfecho = desfechos, fill = list(casos = 0)) %>%
		left_join(meses, by = "mes")

	grafico_mensal_ano <- ggplot(
		mensal_ano,
		aes(x = nome_mes, y = casos, color = desfecho, group = desfecho)
	) +
		geom_line(linewidth = 0.8) +
		geom_point(size = 2) +
		scale_y_continuous(labels = formatar_numero) +
		labs(
			title = paste("Quantidade mensal de tentativas e sucessos -", sufixo),
			x = "Mes",
			y = "Quantidade de notificacoes",
			color = "Desfecho"
		) +
		theme_minimal() +
		theme(axis.text.x = element_text(angle = 45, hjust = 1))

	ggsave(paste0("grafico_quantidade_mensal_", sufixo, ".png"),
		grafico_mensal_ano, width = 10, height = 5, dpi = 300)

	mensal_ano_informado <- mensal_ano %>%
		filter(desfecho != "Sem informacao")

	grafico_mensal_ano_informado <- ggplot(
		mensal_ano_informado,
		aes(x = nome_mes, y = casos, color = desfecho, group = desfecho)
	) +
		geom_line(linewidth = 0.8) +
		geom_point(size = 2) +
		scale_y_continuous(labels = formatar_numero) +
		labs(
			title = paste("Quantidade mensal de tentativas e sucessos -", sufixo),
			subtitle = "Apenas casos com desfecho informado",
			x = "Mes",
			y = "Quantidade de notificacoes",
			color = "Desfecho"
		) +
		theme_minimal() +
		theme(axis.text.x = element_text(angle = 45, hjust = 1))

	ggsave(
		paste0("grafico_quantidade_mensal_desfecho_informado_", sufixo, ".png"),
		grafico_mensal_ano_informado,
		width = 10, height = 5, dpi = 300
	)

	message("Graficos de ", sufixo, " salvos.")
	invisible(NULL)
}

gerar_graficos_ano(2024)
gerar_graficos_ano(2025)

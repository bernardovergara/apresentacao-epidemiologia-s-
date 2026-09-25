for (ano in 15:24) {
  ano_2_digitos <- sprintf("%02d", ano)
  nome_arquivo <- paste0("VIOLBR", ano_2_digitos, ".dbc")
  dir.create(file.path("dados", "brutos"), recursive = TRUE, showWarnings = FALSE)

  message("Baixando ", nome_arquivo, "...")
  download.file(
    paste0(
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/",
      nome_arquivo
    ),
    destfile = file.path("dados", "brutos", nome_arquivo),
    mode = "wb"
  )
}
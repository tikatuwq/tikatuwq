# audit/scripts/testes_externos_0_11_0.R
# Testes EXTERNOS (win-builder e mac builder) do tikatuwq 0.11.0.
#
# ESTE SCRIPT ENVIA O CODIGO DO PACOTE A SERVICOS DE TERCEIROS. Por padrao,
# ele NAO envia nada: so mostra o plano. O envio exige autorizacao explicita
# do mantenedor e, no ambiente, TKW_AUTORIZO_TESTES_EXTERNOS=SIM.
#
# Uso (a partir da raiz de um clone limpo do commit candidato):
#   Rscript audit/scripts/testes_externos_0_11_0.R <tarball> <sha256_esperado> [win-devel|win-release|mac-release]
#
# O que e transmitido:
#   - win-builder (win-builder.r-project.org): o pacote-fonte (tarball). O
#     resultado vai por e-mail ao MANTENEDOR do DESCRIPTION. Os arquivos de
#     resultado ficam num diretorio do servidor acessivel por link durante
#     alguns dias.
#   - mac builder (mac.r-project.org/macbuilder): o pacote-fonte. Devolve um
#     link para os resultados.
# O tarball candidato nao contem registros individuais do IGAM, dados
# privados, caminhos pessoais nem segredos (ver
# audit/RELATORIO_VALIDACAO_CANDIDATO_0_11_0.md).

args <- commandArgs(TRUE)
if (length(args) < 2L) stop("Uso: <tarball> <sha256_esperado> [win-devel|win-release|mac-release]", call. = FALSE)
tarball <- args[1]; esperado <- tolower(args[2])
alvo <- if (length(args) >= 3L) args[3] else "win-devel"
stopifnot(alvo %in% c("win-devel", "win-release", "mac-release"))
if (!file.exists(tarball)) stop("Tarball nao encontrado: ", tarball, call. = FALSE)
sha <- if (requireNamespace("openssl", quietly = TRUE)) {
  tolower(paste(as.vector(unclass(openssl::sha256(file(tarball, open = "rb")))), collapse = ""))
} else {
  stop("Pacote 'openssl' necessario para conferir o SHA-256.", call. = FALSE)
}
if (!isTRUE(sha == esperado)) stop("SHA-256 do tarball (", sha, ") difere do esperado (", esperado, "). Nada enviado.", call. = FALSE)

desc <- read.dcf(file.path(".", "DESCRIPTION"))
cat("Pacote:", desc[, "Package"], desc[, "Version"], "| tarball:", basename(tarball), "| SHA-256 conferido\n")
cat("Alvo:", alvo, "| resultados por e-mail para o mantenedor:", desc[, "Maintainer"], "\n")

if (!identical(Sys.getenv("TKW_AUTORIZO_TESTES_EXTERNOS"), "SIM")) {
  cat("\nMODO PLANO: nenhum envio realizado. Para enviar, com autorizacao do mantenedor:\n",
      "  TKW_AUTORIZO_TESTES_EXTERNOS=SIM Rscript audit/scripts/testes_externos_0_11_0.R ", tarball, " ", esperado, " ", alvo, "\n", sep = "")
  quit(save = "no", status = 0)
}

# ---- Envio (so com autorizacao) --------------------------------------------
# Envia o tarball EXATO conferido acima (nao reconstroi).
if (alvo %in% c("win-devel", "win-release")) {
  pasta <- if (alvo == "win-devel") "R-devel" else "R-release"
  url <- paste0("ftp://win-builder.r-project.org/", pasta, "/", basename(tarball))
  h <- curl::new_handle(upload = TRUE, filetime = FALSE)
  con <- file(tarball, open = "rb"); on.exit(close(con), add = TRUE)
  curl::handle_setopt(h, readfunction = function(n) readBin(con, raw(), n), infilesize = file.size(tarball))
  curl::curl_fetch_memory(url, handle = h)
  cat("Enviado ao win-builder (", pasta, "). Aguarde o e-mail do mantenedor com o link dos resultados.\n", sep = "")
} else {
  # O mac builder recebe o pacote via devtools::check_mac_release(), que
  # reconstroi a partir do diretorio do pacote (mesmo commit).
  devtools::check_mac_release(pkg = ".")
  cat("Enviado ao mac builder. Use o link de resultados exibido acima.\n")
}

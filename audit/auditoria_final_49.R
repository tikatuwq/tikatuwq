# audit/auditoria_final_49.R
# Auditoria final (pre-release 0.11.0): recalcula os 49 IQAs IGAM pela API
# publica iqa(method = "IGAM") a partir dos extratos brutos das planilhas
# oficiais, conferindo antes os extratos contra a planilha original de 2017.
#
# NAO contem dados. Le de <dir_privado> (FORA do repositorio):
#   igam_pampulha_2017q3_entradas.csv, igam_pampulha_validacao_externa_entradas.csv,
#   altitudes_estacoes_pampulha.csv (estacao, altitude_m)
# e grava o resultado por amostra numa subpasta nova de <dir_privado>. Apenas
# as metricas agregadas vao para audit/resultados/auditoria_final_49_metricas.csv.
# Fontes e SHA-1: audit/dados/FONTES.md; ver audit/DADOS_IGAM_PRIVADOS.md.
#
#   Rscript audit/auditoria_final_49.R <dir_privado> [<2017.xlsx>]

suppressMessages(devtools::load_all(".", quiet = TRUE))
a <- commandArgs(TRUE)
if (length(a) < 1L) {
  stop("Uso: Rscript audit/auditoria_final_49.R <dir_privado> [<2017.xlsx>]. Os dados do IGAM ",
       "nao sao versionados; ver audit/DADOS_IGAM_PRIVADOS.md.", call. = FALSE)
}
priv <- normalizePath(a[1], winslash = "/", mustWork = FALSE)
repo <- normalizePath(".", winslash = "/")
if (!dir.exists(priv)) stop("Diretorio privado nao encontrado: ", priv, call. = FALSE)
if (startsWith(tolower(paste0(priv, "/")), tolower(paste0(repo, "/")))) {
  stop("<dir_privado> esta dentro do repositorio; use um diretorio externo.", call. = FALSE)
}
f15 <- file.path(priv, "igam_pampulha_2017q3_entradas.csv")
fext <- file.path(priv, "igam_pampulha_validacao_externa_entradas.csv")
falt <- file.path(priv, "altitudes_estacoes_pampulha.csv")
need <- c(f15, fext, falt, a[-1])
miss <- need[!file.exists(need)]
if (length(miss)) stop("Arquivo(s) de dados IGAM nao encontrado(s): ", paste(miss, collapse = ", "),
                       ". Ver audit/dados/FONTES.md.", call. = FALSE)
a <- a[-1]

# ---- 1. Extrato dos 15 casos (3o trim. 2017) x planilha original -------------
e15 <- read.csv(f15, check.names = FALSE,
                fileEncoding = "UTF-8", stringsAsFactors = FALSE, colClasses = "character")
params <- c("Oxigênio dissolvido", "Escherichia coli", "pH in loco", "Demanda Bioquímica de Oxigênio",
            "Nitrato", "Fósforo total", "Turbidez", "Sólidos totais", "Temperatura da água", "Cloreto total")
if (length(a) >= 1L) {
  x <- as.data.frame(readxl::read_excel(a[1], sheet = "São Francisco", col_types = "text"), check.names = FALSE)
  x$data <- format(as.Date(as.numeric(x[["Data de Amostragem"]]), origin = "1899-12-30"))
  k <- match(paste(e15[["Estação"]], e15$data), paste(x[["Estação"]], x$data))
  stopifnot(!anyNA(k))
  same <- vapply(params, function(p) {
    v1 <- e15[[p]]; v2 <- x[[p]][k]; s1 <- e15[[paste("Sinal", p)]]; s2 <- x[[paste("Sinal", p)]][k]
    all(abs(as.numeric(v1) - as.numeric(v2)) < 1e-12) && identical(is.na(s1) | s1 == "", is.na(s2) | s2 == "")
  }, logical(1))
  cat("Extrato 2017T3 identico a planilha (10 parametros, 15 linhas):", all(same), "\n")
}

# ---- 2. Entradas dos 49 casos a partir dos extratos -------------------------
alt_tab <- utils::read.csv(falt, stringsAsFactors = FALSE)
alt15 <- stats::setNames(alt_tab$altitude_m, alt_tab$estacao)
ext <- read.csv(fext, check.names = FALSE,
                fileEncoding = "UTF-8", stringsAsFactors = FALSE, colClasses = "character")
build <- function(r, campanha, grupo, alt, pub) {
  sg <- function(p) { s <- r[[paste("Sinal", p)]]; v <- r[[p]]; ifelse(is.na(s) | s == "", v, paste0(s, v)) }
  data.frame(id = paste(r[["Estação"]], r$data), campanha = campanha, grupo = grupo,
             estacao = r[["Estação"]], data = r$data, altitude_m = as.numeric(alt),
             od = sg("Oxigênio dissolvido"), temperatura = as.numeric(r[["Temperatura da água"]]),
             cloreto = sg("Cloreto total"), e_coli = sg("Escherichia coli"),
             ph = as.numeric(r[["pH in loco"]]), dbo = sg("Demanda Bioquímica de Oxigênio"),
             nitrato = sg("Nitrato"), p_total = sg("Fósforo total"), turbidez = sg("Turbidez"),
             solidos_totais = sg("Sólidos totais"), IQA_publicado = as.numeric(pub),
             stringsAsFactors = FALSE)
}
d <- rbind(
  build(e15, "Pampulha 3o trim. 2017", "investigacao (15)", alt15[e15[["Estação"]]], e15$IQA_publicado),
  build(ext, ifelse(grepl("2018", ext$conjunto), "Pampulha 3o trim. 2018", "Pampulha 4o trim. 2017"),
        "externa pre-registrada (34)", ext$Altitude, ext$IQA_publicado)
)
stopifnot(nrow(d) == 49L, !anyDuplicated(d$id))

# ---- 3. API publica ----------------------------------------------------------
inp <- d[, c("od", "temperatura", "cloreto", "e_coli", "ph", "dbo", "nitrato", "p_total", "turbidez", "solidos_totais")]
o <- iqa(inp, method = "IGAM", altitude_m = d$altitude_m, nitrate_unit = "N",
         phosphorus_basis = "P", details = TRUE)
cp <- attr(o, "components")
d$IQA_bruto <- cp$IQA_unrounded
d$IQA_bruto_17g <- sprintf("%.17g", cp$IQA_unrounded)
d$IQA_arredondado <- o$IQA
d$classe_IGAM <- as.character(o$IQA_status)
d$erro <- d$IQA_bruto - d$IQA_publicado
d$abs_erro <- abs(d$erro)
d$igual_1casa <- d$IQA_arredondado == d$IQA_publicado
d$dentro_0.05 <- d$abs_erro <= 0.05
d$limitrofe <- d$abs_erro > 0.045
d$dist_desempate <- abs((d$IQA_bruto * 10) %% 1 - 0.5) / 10
d$testa_k <- as.numeric(gsub("[<>]", "", d$turbidez)) <= 100
d$elegivel <- !is.na(d$IQA_bruto)
d$motivo_exclusao <- ifelse(d$elegivel, "", cp$observacao)
saida <- file.path(priv, paste0("saidas_", format(Sys.time(), "%Y%m%d_%H%M%S")))
dir.create(saida)
utils::write.csv(d, file.path(saida, "auditoria_final_49_casos.csv"), row.names = FALSE)
cat("Resultado por amostra gravado fora do repositorio:", saida, "\n")

met <- function(x) data.frame(n = nrow(x), elegiveis = sum(x$elegivel), iguais_1casa = sum(x$igual_1casa),
  dentro_0.05 = sum(x$dentro_0.05), mae = mean(x$abs_erro), rmse = sqrt(mean(x$erro^2)),
  vies = mean(x$erro), max_abs = max(x$abs_erro), limitrofes = sum(x$limitrofe),
  min_dist_desempate = min(x$dist_desempate), testam_k = sum(x$testa_k))
m <- rbind(cbind(grupo = "total (49)", met(d)),
           do.call(rbind, lapply(split(d, d$grupo), function(x) cbind(grupo = x$grupo[1], met(x)))),
           do.call(rbind, lapply(split(d, d$campanha), function(x) cbind(grupo = x$campanha[1], met(x)))))
utils::write.csv(m, "audit/resultados/auditoria_final_49_metricas.csv", row.names = FALSE)
print(m, row.names = FALSE, digits = 5)
# Sem identificadores por amostra no console (os detalhes ficam em <dir_privado>)
cat("\nLimitrofes (|erro| > 0,045):", sum(d$limitrofe), "| faixa de |erro|:",
    paste(sprintf("%.4f", range(d$abs_erro[d$limitrofe])), collapse = " a "), "\n")
cat("\nR", R.version.string, "| tikatuwq", as.character(utils::packageVersion("tikatuwq")),
    "| readxl", as.character(utils::packageVersion("readxl")), "\n")

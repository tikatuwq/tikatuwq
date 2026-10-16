# audit/igam_extrair_validacao_externa.R
# Extrai, sem alterar valores, as entradas das estacoes Pampulha das planilhas
# oficiais "Series Historicas" do IGAM para a validacao externa pre-registrada
# (audit/dados/PRE_REGISTRO_VALIDACAO_EXTERNA.md).
#
# NAO contem dados: estacoes e IQAs publicados sao lidos de
# <dir_privado>/iqa_publicado_validacao_externa.csv (colunas conjunto,
# estacao, IQA_publicado), e o extrato e gravado numa subpasta nova de
# <dir_privado>. <dir_privado> deve ficar FORA do repositorio. Fontes e SHA-1:
# audit/dados/FONTES.md.
#
#   Rscript audit/igam_extrair_validacao_externa.R <dir_privado> <2018_3trim.xlsx> <2017.xlsx> <2018_4trim.xlsx>

a <- commandArgs(TRUE)
if (length(a) != 4L) {
  stop("Uso: Rscript audit/igam_extrair_validacao_externa.R <dir_privado> <2018_3trim.xlsx> ",
       "<2017.xlsx> <2018_4trim.xlsx>. Os dados do IGAM nao sao versionados; ",
       "ver audit/DADOS_IGAM_PRIVADOS.md e audit/dados/FONTES.md.", call. = FALSE)
}
priv <- normalizePath(a[1], winslash = "/", mustWork = FALSE)
repo <- normalizePath(".", winslash = "/")
if (!dir.exists(priv)) stop("Diretorio privado nao encontrado: ", priv, call. = FALSE)
if (startsWith(tolower(paste0(priv, "/")), tolower(paste0(repo, "/")))) {
  stop("<dir_privado> esta dentro do repositorio; use um diretorio externo.", call. = FALSE)
}
pub_file <- file.path(priv, "iqa_publicado_validacao_externa.csv")
need <- c(pub_file, a[2:4])
miss <- need[!file.exists(need)]
if (length(miss)) stop("Arquivo(s) nao encontrado(s): ", paste(miss, collapse = ", "), call. = FALSE)
f_e1 <- a[2]; f_e2 <- a[3]; f_alt <- a[4]

pub <- utils::read.csv(pub_file, stringsAsFactors = FALSE, colClasses = "character")
stations <- sort(unique(pub$estacao))
params <- c("Oxigênio dissolvido", "Escherichia coli", "Coliformes termotolerantes", "pH in loco",
            "Demanda Bioquímica de Oxigênio", "Nitrato", "Fósforo total", "Turbidez",
            "Sólidos totais", "Temperatura da água", "Cloreto total")

read_sf <- function(f) {
  x <- readxl::read_excel(f, sheet = "São Francisco", col_types = "text")
  as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
}
excel_date <- function(v) {
  num <- suppressWarnings(as.numeric(v))
  out <- as.Date(num, origin = "1899-12-30")
  bad <- is.na(num) & !is.na(v)
  out[bad] <- as.Date(substr(v[bad], 1, 10))
  out
}
pick <- function(x, from, to, label) {
  x$data <- excel_date(x[["Data de Amostragem"]])
  x <- x[x[["Estação"]] %in% stations & x$data >= as.Date(from) & x$data <= as.Date(to), , drop = FALSE]
  keep <- c("Estação", "Data de Amostragem", "data", if ("Altitude" %in% names(x)) "Altitude",
            unlist(lapply(params, function(p) c(paste("Sinal", p), p))))
  for (m in setdiff(keep, names(x))) x[[m]] <- NA_character_
  x <- x[, keep, drop = FALSE]
  x$conjunto <- label
  x[order(x[["Estação"]], x$data), , drop = FALSE]
}

e1 <- pick(read_sf(f_e1), "2018-07-01", "2018-09-30", "E1_Pampulha_2018T3")
e2 <- pick(read_sf(f_e2), "2017-10-01", "2017-12-31", "E2_Pampulha_2017T4")

# Altitude: da planilha da campanha; se ausente, da planilha 4o trim. 2018
alt_ref <- read_sf(f_alt)
alt_ref <- unique(alt_ref[alt_ref[["Estação"]] %in% stations, c("Estação", "Altitude")])
fill_alt <- function(x) {
  if (!"Altitude" %in% names(x)) x$Altitude <- NA_character_
  x$altitude_fonte <- ifelse(is.na(x$Altitude), "planilha 4o trim. 2018", "planilha da campanha")
  i <- is.na(x$Altitude)
  x$Altitude[i] <- alt_ref$Altitude[match(x[["Estação"]][i], alt_ref[["Estação"]])]
  x
}
e1 <- fill_alt(e1); e2 <- fill_alt(e2)

key <- function(conj, est) paste(conj, est)
lookup <- function(x) pub$IQA_publicado[match(key(x$conjunto, x[["Estação"]]), key(pub$conjunto, pub$estacao))]
e1$IQA_publicado <- lookup(e1)
e2$IQA_publicado <- lookup(e2)

common <- union(names(e1), names(e2))
for (m in setdiff(common, names(e1))) e1[[m]] <- NA
for (m in setdiff(common, names(e2))) e2[[m]] <- NA
out <- rbind(e1[, common], e2[, common])
out$data <- format(out$data)
saida <- file.path(priv, paste0("saidas_", format(Sys.time(), "%Y%m%d_%H%M%S")))
dir.create(saida)
dest <- file.path(saida, "igam_pampulha_validacao_externa_entradas.csv")
utils::write.csv(out, dest, row.names = FALSE, fileEncoding = "UTF-8")
cat("Extrato gravado fora do repositorio:", dest, "\n")
cat("Registros por conjunto:\n")
print(table(out$conjunto))

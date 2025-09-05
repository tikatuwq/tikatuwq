# Helpers ---------------------------------------------------------------

.to_double_auto <- function(x) {
  # já é numérico?
  if (is.numeric(x)) return(x)
  x <- as.character(x)

  # tenta vírgula
  v1 <- readr::parse_double(
    x,
    locale = readr::locale(decimal_mark = ","),
    na = c("", "NA", "NaN")
  )
  # tenta ponto
  v2 <- readr::parse_double(
    x,
    locale = readr::locale(decimal_mark = "."),
    na = c("", "NA", "NaN")
  )
  # escolhe a conversão com menos NAs
  if (sum(is.na(v1)) <= sum(is.na(v2))) v1 else v2
}

.fix_ph_if_needed <- function(ph_vec) {
  # se pH > 14 e <= 140 (ex.: 72 -> 7.2), divide por 10 e avisa
  idx <- is.finite(ph_vec) & ph_vec > 14 & ph_vec <= 140
  if (any(idx, na.rm = TRUE)) {
    ph_vec[idx] <- ph_vec[idx] / 10
    rlang::warn(
      paste0("pH > 14 ajustado dividindo por 10 em ", sum(idx),
             " linha(s) — verifique vírgula vs. ponto no CSV.")
    )
  }
  ph_vec
}

#' Leitura padrão de dados de qualidade da água
#'
#' Aceita CSV com **vírgula ou ponto** como separador decimal. Lê tudo como
#' texto e converte as colunas numéricas de forma robusta. Ajusta pH fora
#' da faixa (ex.: 72 -> 7.2) como medida de segurança.
#'
#' @param path Caminho do arquivo CSV.
#' @param tz   Fuso para datas (mantido por compatibilidade; não obrigatório).
#' @export
read_wq <- function(path, tz = "America/Bahia") {
  # Detecta o delimitador olhando a primeira linha
  first_line <- readr::read_lines(path, n_max = 1)
  delim <- if (stringr::str_count(first_line, ";") >
               stringr::str_count(first_line, ",")) ";" else ","

  # Lê TUDO como texto (evita parse errado na origem)
  df <- readr::read_delim(
    file = path,
    delim = delim,
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = "UTF-8"),
    trim_ws = TRUE
  )

  # Normaliza nomes de colunas
  names(df) <- names(df) |>
    stringr::str_replace_all("\\s+", "_") |>
    stringr::str_replace_all("[^A-Za-z0-9_]", "") |>
    tolower()

  # Converte parâmetros numéricos (ajuste a lista conforme seu dicionário)
  param_cols <- intersect(
    names(df),
    c("ph","od","turbidez","dbo","coliformes","p_total","ptotal",
      "fosforo_total","temperatura","ec","condutividade","n_nitrato",
      "n_nitrito","amonia","solidos_totais","solidos_suspensos",
      "conducao","qi","iqa","iet","iet_carlson","iet_lamparelli",
      "nsfwqi","vazao")
  )
  if (length(param_cols)) {
    df[param_cols] <- lapply(df[param_cols], .to_double_auto)
  }

  # Ajuste de segurança para pH
  if ("ph" %in% names(df)) {
    df$ph <- .fix_ph_if_needed(df$ph)
  }

  # Datas comuns
  if ("data" %in% names(df)) {
  d1 <- suppressWarnings(lubridate::ymd(df$data))
  d2 <- suppressWarnings(lubridate::dmy(df$data))
  df$data <- dplyr::coalesce(d1, d2)  # mantém classe Date
  }

  tibble::as_tibble(df)
}

#' Padroniza/unifica unidades (esqueleto)
#' @export
clean_units <- function(df, units_map = NULL){ df }

#' Valida presença de colunas mínimas
#' @export
validate_wq <- function(df, required = c("ph","turbidez","od","dbo","nt_total","p_total","tds","temperatura","coliformes")){
  miss <- setdiff(required, names(df))
  if(length(miss)) stop("Colunas ausentes: ", paste(miss, collapse=", "))
  df
}

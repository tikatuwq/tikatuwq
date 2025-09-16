# R/io_clean.R
# Funcoes de IO e limpeza para dados de qualidade da agua
# (ASCII-only para evitar avisos de nao-ASCII no R CMD check)

# Helpers ----------------------------------------------------------------

# Converte texto em numero detectando virgula ou ponto decimal e ignorando unidades
.to_number_auto <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)

  # Heuristica: qual separador aparece mais nas strings?
  n_comma <- sum(grepl("\\d,\\d", x))
  n_dot   <- sum(grepl("\\d\\.\\d", x))

  if (n_comma > n_dot) {
    return(suppressWarnings(readr::parse_number(
      x, locale = readr::locale(decimal_mark = ","), na = c("", "NA", "NaN")
    )))
  }
  if (n_dot > n_comma) {
    return(suppressWarnings(readr::parse_number(
      x, locale = readr::locale(decimal_mark = "."), na = c("", "NA", "NaN")
    )))
  }

  # Empate: tenta os dois; se empatar de novo, prefere ponto
  v1 <- suppressWarnings(readr::parse_number(
    x, locale = readr::locale(decimal_mark = ","), na = c("", "NA", "NaN")
  ))
  v2 <- suppressWarnings(readr::parse_number(
    x, locale = readr::locale(decimal_mark = "."), na = c("", "NA", "NaN")
  ))
  n1 <- sum(is.na(v1)); n2 <- sum(is.na(v2))
  if (n1 < n2) v1 else if (n2 < n1) v2 else v2
}


# Ajuste conservador: se pH vier claro fora da faixa (ex.: 72 -> 7.2)
.fix_ph_if_needed <- function(ph_vec) {
  idx <- is.finite(ph_vec) & ph_vec > 14 & ph_vec <= 140
  if (any(idx, na.rm = TRUE)) {
    ph_vec[idx] <- ph_vec[idx] / 10
    rlang::warn(
      paste0("pH > 14 adjusted by dividing by 10 in ", sum(idx),
             " row(s) - check comma vs. dot decimal separator in the CSV.")
    )
  }
  ph_vec
}

#' Leitura padrao de dados de qualidade da agua
#'
#' Aceita CSV com **virgula ou ponto** como separador decimal e ignora texto
#' de unidades (ex.: "0,04 mg/L"). Lê tudo como texto, normaliza nomes e
#' converte colunas numericas de forma robusta. Aplica um ajuste de seguranca
#' em pH evidentemente fora da faixa (ex.: 72 -> 7.2).
#'
#' @param path Caminho do arquivo CSV.
#' @param tz   Fuso para datas (mantido por compatibilidade; datas sao Date).
#' @return Um `tibble` com colunas normalizadas.
#' @export
#' @importFrom readr read_lines read_delim parse_number cols col_character locale
#' @importFrom stringr str_count str_replace_all
#' @importFrom lubridate ymd dmy
#' @importFrom tibble as_tibble
#' @importFrom dplyr coalesce
read_wq <- function(path, tz = "America/Bahia") {
  # Detecta delimitador (primeira linha)
  first_line <- readr::read_lines(path, n_max = 1)
  delim <- if (stringr::str_count(first_line, ";") >
               stringr::str_count(first_line, ",")) ";" else ","

  # Le tudo como texto (evita parse precoce incorreto)
  df <- readr::read_delim(
    file = path,
    delim = delim,
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = "UTF-8"),
    trim_ws = TRUE
  )

  # Normaliza nomes: espacos -> _, remove nao-alfa, lowercase
  names(df) <- names(df) |>
    stringr::str_replace_all("\\s+", "_") |>
    stringr::str_replace_all("[^A-Za-z0-9_]", "") |>
    tolower()

  # Colunas tipicamente numericas (ajuste conforme seu dicionario)
  param_cols <- intersect(
    names(df),
    c(
      "ph","od","turbidez","dbo","coliformes",
      "p_total","ptotal","fosforo_total",
      "temperatura","ec","condutividade",
      "n_nitrato","n_nitrito","amonia",
      "nt_total","n_total","ntk","nkjeldahl","nitrogenio_total",
      "solidos_totais","solidos_suspensos","tds",
      "conducao","qi","iqa","iet","iet_carlson","iet_lamparelli",
      "nsfwqi","vazao"
    )
  )

  if (length(param_cols)) {
    df[param_cols] <- lapply(df[param_cols], .to_number_auto)
  }

  # Ajuste de seguranca para pH
  if ("ph" %in% names(df)) {
    df$ph <- .fix_ph_if_needed(df$ph)
  }

  # Datas comuns (tenta ymd e dmy; preserva classe Date)
  if ("data" %in% names(df)) {
    d1 <- suppressWarnings(lubridate::ymd(df$data))
    d2 <- suppressWarnings(lubridate::dmy(df$data))
    df$data <- dplyr::coalesce(d1, d2)
  }

  # Identificadores comuns
  if ("ponto" %in% names(df)) df$ponto <- as.character(df$ponto)

  tibble::as_tibble(df)
}

#' Padroniza/unifica unidades (esqueleto)
#'
#' Ponto de extensao para normalizar unidades (mg/L, uS/cm, etc.).
#' Por ora retorna o data.frame sem alteracoes.
#'
#' @param df data.frame/tibble de entrada.
#' @param units_map opcional; mapa de unidades.
#' @return O mesmo `df` (placeholder).
#' @export
clean_units <- function(df, units_map = NULL) {
  df
}

#' Valida presenca de colunas minimas
#'
#' Garante que o conjunto minimo de colunas exista no dataset.
#'
#' @param df data.frame/tibble de entrada.
#' @param required vetor de nomes de colunas obrigatorias.
#' @return O `df` de entrada se estiver valido; caso contrario, erro.
#' @export
validate_wq <- function(
  df,
  required = c("ph","turbidez","od","dbo","nt_total","p_total","tds","temperatura","coliformes")
) {
  miss <- setdiff(required, names(df))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  df
}

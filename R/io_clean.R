# R/io_clean.R
# Funcoes de IO e limpeza para dados de qualidade da agua
# (ASCII-only no codigo; textos/strings podem ser UTF-8)

# ------------------------------------------------------------------------------
# Helpers internos (nao-exportados)
# ------------------------------------------------------------------------------

# Converte texto em numero detectando virgula ou ponto decimal e ignorando unidades
#' @keywords internal
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
#' @keywords internal
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

# ------------------------------------------------------------------------------
# API publica
# ------------------------------------------------------------------------------

#' Read water-quality CSV (robust parsing)
#'
#' @description
#' Reads a CSV file with **comma or semicolon delimiter** and **comma or dot**
#' as decimal mark, ignoring unit suffixes (e.g., `"0,04 mg/L"`). Everything
#' is read as text first, column names are normalized, and likely numeric
#' columns are parsed robustly. A conservative safeguard adjusts obviously
#' out-of-range pH values (e.g., `72 -> 7.2`).
#'
#' @param path Path to the CSV file.
#' @param tz   Time zone for dates (kept for compatibility; dates are `Date`).
#'
#' @returns
#' A tibble with:
#' \itemize{
#'   \item normalized, lowercase column names (spaces to `_`, non-alnum removed);
#'   \item numeric columns parsed ignoring unit strings;
#'   \item \code{data} parsed to \code{Date} (tries \code{ymd} then \code{dmy});
#'   \item \code{ponto} coerced to character (if present).
#' }
#'
#' @section Parsed numeric candidates:
#' \code{c("ph","od","turbidez","dbo","coliformes","p_total","ptotal",
#' "fosforo_total","temperatura","ec","condutividade","n_nitrato","n_nitrito",
#' "amonia","nt_total","n_total","ntk","nkjeldahl","nitrogenio_total",
#' "solidos_totais","solidos_suspensos","tds","conducao","qi","iqa","iet",
#' "iet_carlson","iet_lamparelli","nsfwqi","vazao")}
#'
#' @seealso [clean_units()], [validate_wq()], [conama_check()], [iqa()]
#'
#' @examples
#' \dontrun{
#' # Minimal example (write a small CSV and read it):
#' tmp <- tempfile(fileext = ".csv")
#' writeLines(
#'   c("ponto;data;ph;od;turbidez",
#'     "R1_01;2025-01-20;7,2;6,8;5,1",
#'     "R1_01;21/01/2025;7.1;7.0;4.8 mg/L"),
#'   tmp
#' )
#' x <- read_wq(tmp)
#' str(x)
#' }
#'
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

#' Normalize/standardize units (placeholder)
#'
#' @description
#' Extension point to normalize units (e.g., mg/L, uS/cm). Currently returns
#' \code{df} unchanged.
#'
#' @param df Input data frame / tibble.
#' @param units_map Optional mapping of units.
#'
#' @returns The input \code{df} unchanged (placeholder).
#' @seealso \code{\link[=read_wq]{read_wq()}}
#'
#' @examples
#' clean_units(data.frame(ph = c(7, 7.2), od = c(6.5, 7.0)))
#'
#' @export
clean_units <- function(df, units_map = NULL) {
  df
}

#' Validate presence of required columns
#'
#' @description
#' Ensures a minimal set of columns exists in the dataset; otherwise throws an
#' error listing the missing names.
#'
#' @param df Input data frame / tibble.
#' @param required Character vector of required column names.
#'
#' @returns The input \code{df} if valid; otherwise, an error is thrown.
#'
#' @seealso \code{\link[=read_wq]{read_wq()}}, \code{\link[=conama_check]{conama_check()}}
#'
#' @examples
#' df_ex <- data.frame(
#'   ph = 7, turbidez = 2, od = 7, dbo = 3,
#'   nt_total = 0.8, p_total = 0.05, tds = 150,
#'   temperatura = 24, coliformes = 200
#' )
#' validate_wq(df_ex)
#'
#' @export
validate_wq <- function(
  df,
  required = c("ph","turbidez","od","dbo","nt_total","p_total","tds","temperatura","coliformes")
) {
  miss <- setdiff(required, names(df))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  df
}

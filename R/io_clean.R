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

#' Fix and validate geographic coordinates (lat/lon)
#'
#' @description
#' Normaliza latitude/longitude quando vierem em graus multiplicados por 1e7
#' (padrao de alguns exports de GPS) e invalida valores fora dos limites.
#'
#' @param df data.frame de entrada.
#' @param lat Nome da coluna de latitude (padrao: "lat").
#' @param lon Nome da coluna de longitude (padrao: "lon").
#' @param divisor Se abs(valor) exceder os limites, divide por este numero
#'   (padrao 1e7).
#'
#' @return O \code{df} com \code{lat/lon} corrigidos quando presentes.
#'
#' @examples
#' # d <- data.frame(lat = -155432345, lon = -393212345)
#' # fix_coords(d)
#'
#' @export
fix_coords <- function(df, lat = "lat", lon = "lon", divisor = 1e7) {
  if (!is.data.frame(df)) stop("fix_coords: 'df' must be a data.frame")

  has_lat <- lat %in% names(df)
  has_lon <- lon %in% names(df)
  if (!has_lat && !has_lon) return(df)

  as_num <- function(x) suppressWarnings(as.numeric(x))

  if (has_lat) {
    v <- as_num(df[[lat]])
    # Only normalize when clearly in scaled GPS format (very large magnitude)
    idx_scale <- is.finite(v) & abs(v) > 1e5
    v[idx_scale] <- v[idx_scale] / divisor
    # Invalidate values outside valid range
    v[!(is.na(v) | (v >= -90 & v <= 90))] <- NA_real_
    df[[lat]] <- v
  }
  if (has_lon) {
    v <- as_num(df[[lon]])
    idx_scale <- is.finite(v) & abs(v) > 1e5
    v[idx_scale] <- v[idx_scale] / divisor
    v[!(is.na(v) | (v >= -180 & v <= 180))] <- NA_real_
    df[[lon]] <- v
  }
  df
}

#' Read water-quality CSV (robust parsing)
#'
#' @description
#' Le um CSV com **delimitador virgula ou ponto-e-virgula** e **virgula ou ponto**
#' como separador decimal, ignorando sufixos de unidade (ex.: `"0,04 mg/L"`).
#' Le tudo como texto primeiro, normaliza nomes, e faz parse robusto de colunas
#' numericas. Ajusta pH evidentemente fora de faixa (ex.: `72 -> 7.2`). Opcionalmente
#' normaliza coordenadas geograficas se vierem em graus * 1e7.
#'
#' @param path Caminho para o arquivo CSV.
#' @param tz   Fuso horario para datas (mantido por compatibilidade; datas sao \code{Date}).
#' @param normalize_coords Logico; se \code{TRUE} (padrao) aplica \code{fix_coords()}
#'   em \code{lat/lon}.
#' @param nd_policy Politica para valores censurados (ND/<LD/<LOQ). Opcoes:
#'   \code{"ld2"} (metade do limite, padrao), \code{"ld"} (limite),
#'   \code{"zero"} (0), \code{"na"} (NA_real_).
#'
#' @returns
#' Um tibble com:
#' \itemize{
#'   \item nomes de colunas normalizados (minusculas, espacos -> `_`, sem nao-alfanum);
#'   \item colunas numericas parseadas ignorando strings de unidade;
#'   \item \code{data} parseada para \code{Date} (tenta \code{ymd} e depois \code{dmy});
#'   \item \code{ponto} coerido para \code{character} (quando presente);
#'   \item \code{lat/lon} corrigidos quando \code{normalize_coords = TRUE}.
#' }
#'
#' @section Parsed numeric candidates:
#' \code{c("ph","od","turbidez","dbo","coliformes","p_total","ptotal",
#' "fosforo_total","temperatura","ec","condutividade","n_nitrato","n_nitrito",
#' "amonia","nt_total","n_total","ntk","nkjeldahl","nitrogenio_total",
#' "solidos_totais","solidos_suspensos","tds","conducao","qi","iqa","iet",
#' "iet_carlson","iet_lamparelli","nsfwqi","vazao","lat","lon")}
#'
#' @seealso [clean_units()], [validate_wq()], [conama_check()], [iqa()]
#'
#' @section Valores censurados (ND/<LD/<LOQ):
#' O pacote implementa uma politica explicita para tratamento de valores censurados.
#' Valores como \code{"<0.01"}, \code{"<LD"}, \code{"<LOD"}, \code{"<LOQ"}, \code{"ND"}
#' sao detectados e tratados conforme a politica especificada em \code{nd_policy}.
#' O padrao \code{"ld2"} usa metade do limite de deteccao (recomendacao conservadora).
#'
#' @examples
#' \dontrun{
#' tmp <- tempfile(fileext = ".csv")
#' writeLines(
#'   c("ponto;data;ph;od;turbidez;lat;lon",
#'     "R1_01;2025-01-20;7,2;6,8;5,1;-163456789;-396543210",
#'     "R1_01;21/01/2025;7.1;7.0;4.8 mg/L;-16.3456789;-39.6543210"),
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
read_wq <- function(path, tz = "America/Bahia", normalize_coords = TRUE,
                    nd_policy = c("ld2", "ld", "zero", "na")) {
  nd_policy <- match.arg(nd_policy)
  
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

  # Colunas tipicamente numericas (inclui lat/lon)
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
      "nsfwqi","vazao","lat","lon"
    )
  )
  if (length(param_cols)) {
    # Aplica politica ND/LD primeiro para tratar valores censurados
    # .parse_nd_ld ja faz parse de valores nao censurados tambem
    for (col_name in param_cols) {
      col_data <- df[[col_name]]
      # Aplica .parse_nd_ld que trata tanto valores censurados quanto normais
      df[[col_name]] <- .parse_nd_ld(col_data, ld_policy = nd_policy)
      # Fallback: se algum valor ainda nao foi parseado, tenta .to_number_auto
      na_idx <- is.na(df[[col_name]]) & !is.na(col_data)
      if (any(na_idx, na.rm = TRUE)) {
        fallback_parsed <- .to_number_auto(col_data[na_idx])
        df[[col_name]][na_idx] <- fallback_parsed
      }
    }
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

  # Coordenadas (opcional)
  if (isTRUE(normalize_coords)) {
    df <- fix_coords(df, lat = "lat", lon = "lon", divisor = 1e7)
  }

  # Identificadores comuns
  if ("ponto" %in% names(df)) df$ponto <- as.character(df$ponto)

  tibble::as_tibble(df)
}

#' Normalize/standardize units
#'
#' @description
#' Normalizes units for water quality parameters. Currently handles common
#' conversions (mg/L to µg/L for phosphorus, unit standardization). Also
#' validates expected unit ranges and emits warnings for values outside
#' typical ranges.
#'
#' @param df Input data frame / tibble.
#' @param units_map Optional named list mapping parameter names to target
#'   units (currently used for validation only).
#'
#' @returns The input \code{df} with normalized units. Currently performs:
#'   - Validation of unit ranges (warns if values are outside typical ranges)
#'   - No actual conversions are performed (returns input unchanged)
#'
#' @details
#' This function is designed as an extension point. Future versions may
#' implement actual unit conversions based on metadata or user specifications.
#'
#' @seealso \code{\link[=read_wq]{read_wq()}}
#'
#' @examples
#' df <- data.frame(ph = c(7, 7.2), od = c(6.5, 7.0), p_total = c(0.05, 0.08))
#' clean_units(df)
#'
#' @importFrom rlang warn
#' @importFrom tibble tribble
#' @export
clean_units <- function(df, units_map = NULL) {
  if (!is.data.frame(df)) {
    stop("clean_units: 'df' must be a data.frame or tibble")
  }

  # Validate typical ranges and warn on outliers (informative only)
  # pH: typically 0-14 (but allow 0-16 for edge cases)
  if ("ph" %in% names(df) || "pH" %in% names(df)) {
    ph_col <- if ("ph" %in% names(df)) "ph" else "pH"
    ph_vals <- suppressWarnings(as.numeric(df[[ph_col]]))
    outliers <- is.finite(ph_vals) & (ph_vals < 0 | ph_vals > 16)
    if (any(outliers, na.rm = TRUE)) {
      rlang::warn(
        paste0("pH values outside typical range (0-14) detected in ",
               sum(outliers, na.rm = TRUE), " row(s). Check data quality.")
      )
    }
  }

  # OD: typically 0-15 mg/L at sea level
  if ("od" %in% names(df)) {
    od_vals <- suppressWarnings(as.numeric(df$od))
    outliers <- is.finite(od_vals) & (od_vals < 0 | od_vals > 20)
    if (any(outliers, na.rm = TRUE)) {
      rlang::warn(
        paste0("OD values outside typical range (0-15 mg/L) detected in ",
               sum(outliers, na.rm = TRUE), " row(s). Check units or data quality.")
      )
    }
  }

  # Turbidez: typically 0-1000 NTU (but can be higher)
  if ("turbidez" %in% names(df)) {
    turb_vals <- suppressWarnings(as.numeric(df$turbidez))
    outliers <- is.finite(turb_vals) & turb_vals < 0
    if (any(outliers, na.rm = TRUE)) {
      rlang::warn(
        paste0("Negative turbidity values detected in ",
               sum(outliers, na.rm = TRUE), " row(s). Check data quality.")
      )
    }
  }

  # p_total: typically 0-10 mg/L (but can be higher)
  if ("p_total" %in% names(df)) {
    pt_vals <- suppressWarnings(as.numeric(df$p_total))
    outliers <- is.finite(pt_vals) & pt_vals < 0
    if (any(outliers, na.rm = TRUE)) {
      rlang::warn(
        paste0("Negative p_total values detected in ",
               sum(outliers, na.rm = TRUE), " row(s). Check data quality.")
      )
    }
  }

  # Tabela interna de unidades padrao e conversoes
  # Unidades padrao (como esperadas pelos indices e CONAMA)
  units_table <- tibble::tribble(
    ~parametro, ~unidade_padrao, ~unidades_aceitas, ~fator_conversao,
    "p_total", "mg/L", "ug/L", 1/1000,  # ug/L -> mg/L: divide por 1000
    "p_total", "mg/L", "ppm", 1,         # ppm = mg/L para agua
    "nt_total", "mg/L", "ug/L", 1/1000,
    "nt_total", "mg/L", "ppm", 1,
    "od", "mg/L", "ppm", 1,
    "od", "mg/L", "ug/L", 1/1000,
    "dbo", "mg/L", "ppm", 1,
    "turbidez", "NTU", "FTU", 1,
    "turbidez", "NTU", "JTU", 1,
    "temperatura", "C", "F", function(x) (x - 32) * 5/9,  # F -> C
    "temperatura", "C", "K", function(x) x - 273.15,      # K -> C
    "condutividade", "us/cm", "mS/cm", 1000,  # mS/cm -> us/cm: multiplica por 1000
    "ec", "us/cm", "mS/cm", 1000
  )
  
  # Se units_map fornecido, usa ele; senao tenta detectar automaticamente
  if (!is.null(units_map) && is.list(units_map)) {
    # Aplica conversoes do units_map
    for (param in names(units_map)) {
      if (param %in% names(df) && is.numeric(df[[param]])) {
        target_unit <- units_map[[param]]
        # Busca fator de conversao na tabela
        conv_row <- units_table[units_table$parametro == param & 
                                units_table$unidades_aceitas == target_unit, , drop = FALSE]
        if (nrow(conv_row) > 0) {
          factor <- conv_row$fator_conversao[[1]]
          if (is.function(factor)) {
            df[[param]] <- factor(df[[param]])
          } else {
            df[[param]] <- df[[param]] * factor
          }
          rlang::warn(
            paste0("Converted column '", param, "' from ", target_unit,
                   " to ", conv_row$unidade_padrao[1], ".")
          )
        }
      }
    }
  } else {
    # Detecta conversoes automaticas baseado em ranges suspeitos
    # p_total: se valores muito altos (>10), pode estar em ug/L
    if ("p_total" %in% names(df)) {
      pt_vals <- suppressWarnings(as.numeric(df$p_total))
      # Se valores tipicamente de ug/L (ex.: >10 mg/L seriam suspeitos para agua doce)
      # Valores >5 mg/L podem indicar unidade errada (ug/L ao inves de mg/L)
      high_idx <- is.finite(pt_vals) & pt_vals > 5 & pt_vals < 10000
      if (any(high_idx, na.rm = TRUE)) {
        # Avisa mas nao converte automaticamente (pode ser verdadeiro valor alto)
        rlang::warn(
          paste0("p_total values >5 mg/L detected. ",
                 "If values are in ug/L, specify units_map = list(p_total = 'ug/L') ",
                 "to convert to mg/L.")
        )
      }
    }
    
    # temperatura: detecta F ou K baseado em ranges
    if ("temperatura" %in% names(df)) {
      temp_vals <- suppressWarnings(as.numeric(df$temperatura))
      # Fahrenheit: tipicamente 32-100 F para agua natural
      f_idx <- is.finite(temp_vals) & temp_vals > 30 & temp_vals < 105 & temp_vals < 50
      if (any(f_idx, na.rm = TRUE)) {
        # Pode ser F, mas nao converte automaticamente
        # (50 F = 10 C, pode ser valido)
      }
      # Kelvin: tipicamente >270 K
      k_idx <- is.finite(temp_vals) & temp_vals > 270 & temp_vals < 350
      if (any(k_idx, na.rm = TRUE)) {
        # Pode ser K, avisa mas nao converte automaticamente
        rlang::warn(
          paste0("Temperatura values suggest Kelvin (K). ",
                 "If so, specify units_map = list(temperatura = 'K') to convert to C.")
        )
      }
    }
  }
  
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
#' @param df Input data.frame/tibble to validate.
#' @param required Character vector of required column names to check for.
#' @param nd_policy Policy for censored values (ND/<LD/<LOQ) when required
#'   columns are not numeric. One of:
#'   - "ld2" (default): use half the detection limit
#'   - "ld"           : use the detection limit
#'   - "zero"         : replace with 0
#'   - "na"           : replace with NA
#'
#' @export
validate_wq <- function(
  df,
  required = c("ph","turbidez","od","dbo","nt_total","p_total","tds","temperatura","coliformes"),
  nd_policy = c("ld2", "ld", "zero", "na")
) {
  nd_policy <- match.arg(nd_policy)
  miss <- setdiff(required, names(df))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  
  # Se algum valor requerido ainda esta como character (possivel censurado), aplica politica
  for (col in required) {
    if (col %in% names(df) && !is.numeric(df[[col]])) {
      orig_col <- df[[col]]
      df[[col]] <- .parse_nd_ld(df[[col]], ld_policy = nd_policy)
      # Fallback para parse normal se ainda houver NA onde havia valor
      na_idx <- is.na(df[[col]]) & !is.na(orig_col)
      if (any(na_idx, na.rm = TRUE)) {
        df[[col]][na_idx] <- .to_number_auto(orig_col[na_idx])
      }
    }
  }
  
  df
}

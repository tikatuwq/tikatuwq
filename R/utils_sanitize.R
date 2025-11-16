# R/utils_sanitize.R
# Helpers internos de saneamento de entrada
# (ASCII-only; nao exportar)

#' @keywords internal
#' @noRd
.numify <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.numeric(x)) return(x)
  if (is.factor(x)) x <- as.character(x)
  x <- gsub("\\s", "", x)
  x <- gsub(",", ".", x, fixed = TRUE)  # virgula -> ponto
  x <- gsub("[<>]", "", x)              # remove sinais de desigualdade
  suppressWarnings(as.numeric(x))
}

#' Parse valores censurados (ND/<LD/<LOQ)
#'
#' @description
#' Helper interno que detecta e trata valores censurados (ex.: "<0.01", "<LD", "<LOQ").
#'
#' @param x Vetor de entrada (character ou numeric).
#' @param ld_policy Politica a aplicar: "ld2" (metade do limite), "ld" (limite),
#'   "zero" (0), "na" (NA_real_).
#' @return Vetor numerico com valores tratados conforme politica.
#' @keywords internal
#' @noRd
.parse_nd_ld <- function(x, ld_policy = c("ld2", "ld", "zero", "na")) {
  ld_policy <- match.arg(ld_policy)
  
  if (is.null(x) || length(x) == 0) return(x)
  if (is.numeric(x)) return(x)
  
  x_char <- as.character(x)
  x_out <- rep(NA_real_, length(x_char))
  censored_flags <- logical(length(x_char))
  
  for (i in seq_along(x_char)) {
    if (is.na(x_char[i])) {
      x_out[i] <- NA_real_
      next
    }
    
    trimmed <- trimws(x_char[i])
    
    # Tenta extrair valor numerico de padrao <valor>
    match_val <- grepl("^\\s*[<>]\\s*([0-9]+[.,]?[0-9]*)\\s*$", trimmed, perl = TRUE)
    if (match_val) {
      # Extrai o numero (substitui virgula por ponto)
      num_str <- gsub("^\\s*[<>]\\s*([0-9]+)[.,]([0-9]*)\\s*$", "\\1.\\2", trimmed, perl = TRUE)
      num_str <- gsub("^\\s*[<>]\\s*([0-9]+)\\s*$", "\\1", num_str, perl = TRUE)
      num_str <- gsub(",", ".", num_str, fixed = TRUE)
      ld_value <- suppressWarnings(as.numeric(num_str))
      
      if (is.finite(ld_value)) {
        censored_flags[i] <- TRUE
        if (ld_policy == "ld2") {
          x_out[i] <- ld_value / 2
        } else if (ld_policy == "ld") {
          x_out[i] <- ld_value
        } else if (ld_policy == "zero") {
          x_out[i] <- 0
        } else {  # "na"
          x_out[i] <- NA_real_
        }
        next
      }
    }
    
    # Tenta detectar <LD, <LOD, <LOQ, ND sem valor numerico
    is_censored <- grepl("^\\s*[<>]\\s*(LD|LOD|LOQ)\\s*$", trimmed, ignore.case = TRUE, perl = TRUE) ||
                   grepl("^\\s*ND\\s*$", trimmed, ignore.case = TRUE, perl = TRUE) ||
                   grepl("^\\s*[<>]\\s*(LD|LOD|LOQ)\\s+([0-9]+[.,]?[0-9]*)\\s*$", trimmed, ignore.case = TRUE, perl = TRUE)
    
    if (is_censored) {
      # Tenta extrair valor apos LD/LOD/LOQ
      ld_match <- regmatches(trimmed, regexec("^\\s*[<>]\\s*(?:LD|LOD|LOQ)\\s+([0-9]+[.,]?[0-9]*)\\s*$", trimmed, ignore.case = TRUE, perl = TRUE))
      if (length(ld_match[[1]]) > 1) {
        num_str <- gsub(",", ".", ld_match[[1]][2], fixed = TRUE)
        ld_value <- suppressWarnings(as.numeric(num_str))
        if (is.finite(ld_value)) {
          censored_flags[i] <- TRUE
          if (ld_policy == "ld2") {
            x_out[i] <- ld_value / 2
          } else if (ld_policy == "ld") {
            x_out[i] <- ld_value
          } else if (ld_policy == "zero") {
            x_out[i] <- 0
          } else {
            x_out[i] <- NA_real_
          }
          next
        }
      }
      
      # Se nao tem valor numerico, usa 0 ou NA conforme politica
      censored_flags[i] <- TRUE
      if (ld_policy == "zero") {
        x_out[i] <- 0
      } else {
        x_out[i] <- NA_real_
      }
      next
    }
    
    # Se nao e censurado, tenta converter para numero normalmente
    num_str <- gsub(",", ".", trimmed, fixed = TRUE)
    x_out[i] <- suppressWarnings(as.numeric(num_str))
  }
  
  # Se havia censura e politica e "ld2", pode avisar (opcional, silencioso por agora)
  # attr(x_out, "censored") <- censored_flags
  
  x_out
}

# extrai vetores numericos p/ IET (com aliases e conversoes)
#' @keywords internal
#' @noRd
.df_extract_iet <- function(df) {
  secchi    <- if ("secchi"    %in% names(df)) .numify(df[["secchi"]])    else if ("sd" %in% names(df)) .numify(df[["sd"]]) else NULL
  clorofila <- if ("clorofila" %in% names(df)) .numify(df[["clorofila"]]) else if ("chla" %in% names(df)) .numify(df[["chla"]]) else NULL
  # tp (ug/L) direto ou derivado de p_total (mg/L)
  tp <- if ("tp" %in% names(df)) {
    .numify(df[["tp"]])
  } else if ("p_total" %in% names(df)) {
    1000 * .numify(df[["p_total"]])   # mg/L -> ug/L
  } else NULL

  ids <- df[, intersect(c("rio","ponto","data","lat","lon"), names(df)), drop = FALSE]
  list(secchi = secchi, clorofila = clorofila, tp = tp, ids = ids)
}

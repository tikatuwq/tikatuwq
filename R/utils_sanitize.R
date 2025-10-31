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

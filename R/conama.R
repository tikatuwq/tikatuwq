#' Limites CONAMA 357/2005 (CSV interno)
#' @export
conama_limits <- function(){
  path <- system.file("extdata","conama_limits.csv", package = "tikatuwq")
  df <- readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
  df$classe <- as.character(df$classe)
  df
}

#' Checagem de conformidade (Classe padrÃƒÂ£o = "2")
#' @export
conama_check <- function(df, classe = "2"){
  lim <- conama_limits() |> dplyr::filter(.data$classe == !!as.character(classe))
  for (p in intersect(names(df), lim$parametro)) {
    rows <- lim[lim$parametro == p, ]
    ok_any <- rep(FALSE, nrow(df))
    for (k in seq_len(nrow(rows))) {
      mi <- suppressWarnings(as.numeric(rows$min[k])); ma <- suppressWarnings(as.numeric(rows$max[k]))
      ok_min <- is.na(mi) | (!is.na(df[[p]]) & df[[p]] >= mi)
      ok_max <- is.na(ma) | (!is.na(df[[p]]) & df[[p]] <= ma)
      ok_any <- ok_any | (ok_min & ok_max)
    }
    df[[paste0(p, "_ok")]] <- as.logical(ok_any)
  }
  df
}

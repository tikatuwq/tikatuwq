#' SumÃ¡rios descritivos por grupo
#' @export
resume_wq <- function(df, by = c("ponto","mes"),
                      funs = c("mean","median","sd")){
  by <- intersect(by, names(df))
  if(!length(by)) stop("Colunas de agrupamento nÃ£o encontradas.")
  numcols <- names(df)[sapply(df, is.numeric)]
  dplyr::group_by(df, dplyr::across(all_of(by))) |>
    dplyr::summarise(dplyr::across(all_of(numcols),
                                   list(mean = mean, median = median, sd = sd),
                                   .names = "{.col}_{.fn}"),
                     .groups = "drop")
}

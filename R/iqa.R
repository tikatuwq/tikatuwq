#' Water Quality Index (WQI / IQA)
#'
#' @description Computes IQA/WQI for each site/date (or grouping).
#'
#' @param df Data frame with the required parameters.
#' @param na_rm Logical; remove NAs before aggregation?
#'
#' @return A tibble/data frame with columns such as \code{site}, \code{date},
#'   \code{iqa} (numeric score), and \code{class} (qualitative label).
#'
#' @export
iqa <- function(df,
  pesos = c(od=.17, coliformes=.15, dbo=.10, nt_total=.10, p_total=.10,
            turbidez=.08, tds=.08, pH=.12, temperatura=.10),
  method = c("CETESB_approx"),
  na_rm = FALSE){

  method <- match.arg(method)
  req <- names(pesos)
  if(!all(req %in% names(df))){
    stop("Missing required columns: ", paste(setdiff(req, names(df)), collapse=", "))
  }

  curves <- iqa_curve_table(method = method)
  qi_col <- function(param) qi_interp(df[[param]], curves[[param]])
  qi_df <- as.data.frame(lapply(names(pesos), qi_col))
  names(qi_df) <- names(pesos)

  if (!na_rm && any(is.na(qi_df))) stop("There are NA values in parameters. Use na_rm=TRUE to ignore incomplete rows.")

  w <- matrix(rep(unname(pesos), each = nrow(qi_df)), ncol = length(pesos))
  denom <- rowSums(!is.na(qi_df) * rep(unname(pesos), each = nrow(qi_df)))
  iqa_val <- rowSums(qi_df * w, na.rm = na_rm) / denom

  df$IQA <- iqa_val
  attr(df, "iqa_method") <- method
  df
}

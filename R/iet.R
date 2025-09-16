#' Trophic State Index (Carlson) - basic
#'
#' @param secchi Numeric vector with Secchi depth (m)
#' @param clorofila Numeric vector with chlorophyll-a (µg/L)
#' @param tp Numeric vector with total phosphorus (µg/L)
#' @return Data frame with TSI components and overall IET
#' @export
iet_carlson <- function(secchi = NULL, clorofila = NULL, tp = NULL){
  res <- list()
  if (!is.null(secchi)) res$TSI_Secchi <- 60 - 14.41 * log(secchi, base=10)
  if (!is.null(clorofila)) res$TSI_Chla <- 9.81 * log(clorofila, base=10) + 30.6
  if (!is.null(tp)) res$TSI_TP <- 14.42 * log(tp, base=10) + 4.15
  df <- as.data.frame(res)
  if(ncol(df) > 1){
    df$IET <- rowMeans(df, na.rm = TRUE)
  } else if(ncol(df) == 1){
    df$IET <- df[[1]]
  }
  df
}

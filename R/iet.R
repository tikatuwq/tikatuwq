#' Trophic State Index (Carlson)
#'
#' @description
#' Computes Carlson's Trophic State Index (TSI/IET) from Secchi depth,
#' chlorophyll-a, and total phosphorus. Returns component scores and the
#' overall IET as the row-wise mean of available components.
#'
#' @param secchi Numeric vector with Secchi depth (m).
#' @param clorofila Numeric vector with chlorophyll-a (ug/L).
#' @param tp Numeric vector with total phosphorus (ug/L).
#'
#' @details
#' Implemented component formulas (Carlson 1977):
#' \itemize{
#'   \item \code{TSI_Secchi = 60 - 14.41 * log10(secchi)}
#'   \item \code{TSI_Chla  = 9.81 * log10(clorofila) + 30.6}
#'   \item \code{TSI_TP    = 14.42 * log10(tp) + 4.15}
#' }
#'
#' Inputs may contain \code{NA} and are recycled according to R rules.
#' The overall index \code{IET} is the row mean of the available components
#' (\code{na.rm = TRUE}).
#'
#' @returns
#' A data frame with columns (when applicable):
#' \itemize{
#'   \item \code{TSI_Secchi} — component from Secchi depth.
#'   \item \code{TSI_Chla} — component from chlorophyll-a.
#'   \item \code{TSI_TP} — component from total phosphorus.
#'   \item \code{IET} — overall Carlson index (row mean).
#' }
#'
#' @references
#' Carlson, R. E. (1977). \emph{A trophic state index for lakes}.
#' Limnology and Oceanography, 22(2), 361–369.
#' doi:10.4319/lo.1977.22.2.0361
#'
#' @seealso \code{\link[=iet_lamparelli]{iet_lamparelli()}}, \code{\link[=iqa]{iqa()}}, \code{\link[=conama_check]{conama_check()}}
#'
#' @examples
#' # Example data
#' secchi <- c(1.2, 0.8, 0.4)        # m
#' clorofila <- c(5, 12, 30)         # ug/L
#' tp <- c(20, 40, 70)               # ug/L
#'
#' iet_carlson(secchi = secchi, clorofila = clorofila, tp = tp)
#'
#' # With only one component
#' iet_carlson(secchi = secchi)
#'
#' @export
iet_carlson <- function(secchi = NULL, clorofila = NULL, tp = NULL) {
  res <- list()
  if (!is.null(secchi))    res$TSI_Secchi <- 60 - 14.41 * log10(pmax(secchi, 0.001))
  if (!is.null(clorofila)) res$TSI_Chla  <- 9.81 * log10(pmax(clorofila, 0.001)) + 30.6
  if (!is.null(tp))        res$TSI_TP    <- 14.42 * log10(pmax(tp, 0.001)) + 4.15

  df <- as.data.frame(res, optional = TRUE)

  if (ncol(df) > 1) {
    df$IET <- rowMeans(df, na.rm = TRUE)
  } else if (ncol(df) == 1) {
    df$IET <- df[[1]]
  } else {
    df <- data.frame(IET = numeric(0))
  }

  df
}

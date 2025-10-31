#' Trophic State Index (Carlson)
#'
#' @description
#' Computes Carlson's Trophic State Index (TSI/IET) from Secchi depth,
#' chlorophyll-a, and total phosphorus. Returns component scores and the
#' overall IET as the row-wise mean of available components.
#'
#' You can also pass a data.frame as the first argument (see Details).
#'
#' @param secchi Numeric vector with Secchi depth (m) **or** a data.frame
#'   containing columns named \code{secchi} (m), \code{clorofila} (ug/L) and
#'   \code{tp} (ug/L) or \code{p_total} (mg/L). If a data.frame is provided,
#'   \code{clorofila} and \code{tp} must be \code{NULL}.
#' @param clorofila Numeric vector with chlorophyll-a (ug/L).
#' @param tp Numeric vector with total phosphorus (ug/L).
#' @param .keep_ids Logical; when a data.frame is provided, bind back common
#'   ID columns (\code{rio}, \code{ponto}, \code{data}, \code{lat}, \code{lon}).
#'   Default \code{FALSE} (keeps historical behaviour).
#'
#' @details
#' Implemented component formulas (Carlson 1977):
#' \itemize{
#'   \item \code{TSI_Secchi = 60 - 14.41 * log10(secchi)}
#'   \item \code{TSI_Chla  = 9.81 * log10(clorofila) + 30.6}
#'   \item \code{TSI_TP    = 14.42 * log10(tp) + 4.15}
#' }
#'
#' When a data.frame is provided, character inputs using comma decimal (e.g. "3,2")
#' or inequality symbols (e.g. "<0,1") are safely converted to numeric. If
#' \code{p_total} (mg/L) exists instead of \code{tp} (ug/L), it is converted
#' internally (\code{tp = p_total * 1000}).
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
#' # Vector usage (kept as-is)
#' secchi <- c(1.2, 0.8, 0.4)        # m
#' clorofila <- c(5, 12, 30)         # ug/L
#' tp <- c(20, 40, 70)               # ug/L
#' iet_carlson(secchi = secchi, clorofila = clorofila, tp = tp)
#'
#' # Data frame usage (optional)
#' # df <- data.frame(secchi = secchi, clorofila = clorofila, p_total = c(0.02, 0.04, 0.07))
#' # iet_carlson(df)                  # auto-converts p_total (mg/L) to tp (ug/L)
#' # iet_carlson(df, .keep_ids = TRUE)
#'
#' @export
iet_carlson <- function(secchi = NULL, clorofila = NULL, tp = NULL, .keep_ids = FALSE) {

  # --- modo data.frame (opcional): secchi é um DF e demais NULL ---
  if (is.data.frame(secchi) && is.null(clorofila) && is.null(tp)) {
    ext <- .df_extract_iet(secchi)
    res <- list()
    if (!is.null(ext$secchi))    res$TSI_Secchi <- 60 - 14.41 * log10(pmax(ext$secchi, 0.001))
    if (!is.null(ext$clorofila)) res$TSI_Chla  <- 9.81 * log10(pmax(ext$clorofila, 0.001)) + 30.6
    if (!is.null(ext$tp))        res$TSI_TP    <- 14.42 * log10(pmax(ext$tp, 0.001)) + 4.15

    df_out <- as.data.frame(res, optional = TRUE)
    if (ncol(df_out) > 1) {
      df_out$IET <- rowMeans(df_out, na.rm = TRUE)
    } else if (ncol(df_out) == 1) {
      df_out$IET <- df_out[[1]]
    } else {
      df_out <- data.frame(IET = numeric(0))
    }
    if (.keep_ids && nrow(ext$ids)) df_out <- cbind(ext$ids, df_out)
    return(df_out)
  }

  # --- modo tradicional (vetorial) — preservado ---
  secchi    <- .numify(secchi)
  clorofila <- .numify(clorofila)
  tp        <- .numify(tp)

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

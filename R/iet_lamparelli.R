#' Trophic State Index (Lamparelli)
#'
#' @description
#' Computes components of the Lamparelli trophic state index (TSI/IET) from
#' total phosphorus, chlorophyll-a, and Secchi depth, and returns the overall
#' Lamparelli index as the row-wise mean of available components.
#'
#' You can also pass a data.frame as the first argument (see Details).
#'
#' @param tp Numeric total phosphorus (mg/L) **or** a data.frame containing
#'   columns named \code{tp} (ug/L) or \code{p_total} (mg/L), \code{chla} or
#'   \code{clorofila} (ug/L), and \code{sd} or \code{secchi} (m). If a
#'   data.frame is provided, \code{chla} and \code{sd} must be \code{NULL}.
#' @param chla Numeric chlorophyll-a (ug/L).
#' @param sd Numeric Secchi disk depth (m).
#' @param ambiente Character, environment type: 'rio' or 'reservatorio'.
#' @param .keep_ids Logical; when a data.frame is provided, bind back common ID
#'   columns (\code{rio}, \code{ponto}, \code{data}, \code{lat}, \code{lon}).
#'   Default FALSE (preserves historical behaviour).
#'
#' @details
#' Minimal, pragmatic implementation; confirm coefficients/thresholds for your
#' region before regulatory use. Character inputs like "3,2" or "<0,1" are
#' safely converted. If only \code{p_total} (mg/L) is present, it is converted
#' to \code{tp} in ug/L via \code{tp = p_total * 1000}.
#'
#' @returns Data frame with IET components and overall \code{IET_Lamp}.
#'
#' @export
iet_lamparelli <- function(tp = NULL, chla = NULL, sd = NULL,
                           ambiente = c("rio", "reservatorio"),
                           .keep_ids = FALSE) {
  ambiente <- match.arg(ambiente)

  # Usa helper interno .numify() (R/utils_sanitize.R)

  # --- MODO DATA.FRAME (opcional): tp é DF e demais NULL ---
  if (is.data.frame(tp) && is.null(chla) && is.null(sd)) {
    ext <- .df_extract_iet(tp)
    res <- list()
    if (!is.null(ext$tp))        res$IET_TP     <- 10 + 10 * log10(pmax(ext$tp,   0.001))
    if (!is.null(ext$clorofila)) res$IET_Chla   <- 10 + 10 * log10(pmax(ext$clorofila, 0.001))
    if (!is.null(ext$secchi))    res$IET_Secchi <- 60 - 14.41 * log10(pmax(ext$secchi, 0.001))

    df <- as.data.frame(res, optional = TRUE)
    if (ncol(df) > 1) {
      df$IET_Lamp <- rowMeans(df, na.rm = TRUE)
    } else if (ncol(df) == 1) {
      df$IET_Lamp <- df[[1]]
    } else {
      df <- data.frame(IET_Lamp = numeric(0))
    }
    df$ambiente <- ambiente
    if (.keep_ids && nrow(ext$ids)) df <- cbind(ext$ids, df)
    return(df)
  }

  # --- MODO VETORIAL (comportamento original) ---
  tp   <- .numify(tp)      # mg/L
  chla <- .numify(chla)    # ug/L
  sd   <- .numify(sd)      # m

  res <- list()
  if (!is.null(tp))   res$IET_TP     <- 10 + 10 * log10(pmax(tp,   0.001))
  if (!is.null(chla)) res$IET_Chla   <- 10 + 10 * log10(pmax(chla, 0.001))
  if (!is.null(sd))   res$IET_Secchi <- 60 - 14.41 * log10(pmax(sd, 0.001))

  df <- as.data.frame(res, optional = TRUE)

  if (ncol(df) > 1) {
    df$IET_Lamp <- rowMeans(df, na.rm = TRUE)
  } else if (ncol(df) == 1) {
    df$IET_Lamp <- df[[1]]
  } else {
    df <- data.frame(IET_Lamp = numeric(0))
  }

  df$ambiente <- ambiente
  df
}

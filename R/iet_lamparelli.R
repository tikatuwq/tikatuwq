#' Trophic State Index (Lamparelli)
#'
#' @description
#' Computes components of the Lamparelli trophic state index (TSI/IET) from
#' total phosphorus, chlorophyll-a, and Secchi depth, and returns the overall
#' Lamparelli index as the row-wise mean of available components.
#'
#' @param tp Numeric total phosphorus (mg/L).
#' @param chla Numeric chlorophyll-a (ug/L).
#' @param sd Numeric Secchi disk depth (m).
#' @param ambiente Character, environment type: `'rio'` or `'reservatorio'`.
#'
#' @details
#' Implemented component formulas (simple skeleton):
#' \itemize{
#'   \item \code{IET_TP = 10 + 10 * log10(max(tp, 0.001))}
#'   \item \code{IET_Chla = 10 + 10 * log10(max(chla, 0.001))}
#'   \item \code{IET_Secchi = 60 - 14.41 * log10(max(sd, 0.001))}
#' }
#' The overall \code{IET_Lamp} is the row mean of available components
#' (\code{na.rm = TRUE}). Inputs are recycled by standard R vector recycling
#' rules and can contain \code{NA}.
#'
#' This is a minimal, pragmatic implementation intended for quick summaries;
#' practitioners should confirm the most appropriate equations/coefficients for
#' the specific waterbody type and region before regulatory use.
#'
#' @returns
#' A data frame with columns (when applicable):
#' \itemize{
#'   \item \code{IET_TP} — component from total phosphorus.
#'   \item \code{IET_Chla} — component from chlorophyll-a.
#'   \item \code{IET_Secchi} — component from Secchi depth.
#'   \item \code{IET_Lamp} — overall Lamparelli index (row mean).
#'   \item \code{ambiente} — the informed environment label.
#' }
#'
#' @references
#' Carlson, R. E. (1977). \emph{A trophic state index for lakes}. Limnology
#' and Oceanography, 22(2), 361–369. doi:10.4319/lo.1977.22.2.0361
#'
#' Lamparelli, M. C. (2004). \emph{Graus de trofia em corpos d’água do Estado
#' de São Paulo}. (Tese de Doutorado). Instituto de Biociências, USP.
#'
#' @seealso \code{\link[=iet_carlson]{iet_carlson()}}, \code{\link[=iqa]{iqa()}}, \code{\link[=conama_check]{conama_check()}}
#'
#' @examples
#' # Vectors (can include NA)
#' tp   <- c(0.02, 0.05, 0.10)        # mg/L
#' chla <- c(5, 12, 30)               # ug/L
#' sd   <- c(1.2, 0.8, 0.4)           # m
#'
#' iet_lamparelli(tp = tp, chla = chla, sd = sd, ambiente = "reservatorio")
#'
#' # With a single component:
#' iet_lamparelli(tp = tp, ambiente = "rio")
#'
#' @export
iet_lamparelli <- function(tp = NULL, chla = NULL, sd = NULL,
                           ambiente = c("rio", "reservatorio")) {
  ambiente <- match.arg(ambiente)

  # Build component columns only for provided inputs
  res <- list()
  if (!is.null(tp))   res$IET_TP    <- 10 + 10 * log10(pmax(tp,   0.001))
  if (!is.null(chla)) res$IET_Chla  <- 10 + 10 * log10(pmax(chla, 0.001))
  if (!is.null(sd))   res$IET_Secchi<- 60 - 14.41 * log10(pmax(sd, 0.001))

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

# R/iqa_curves.R
# Curvas e interpolacao de Qi (IQA) - helpers internos
# (ASCII-only no codigo)

#' Lookup tables for Qi (IQA) by parameter (CETESB/NSF-like, approximate)
#'
#' @description
#' Returns a list of small data frames used to map raw measurements (x)
#' to quality sub-scores (qi) per parameter. These tables are intended
#' for piecewise-linear interpolation and are **approximate**.
#'
#' @param method Character scalar. Currently only `"CETESB_approx"`.
#'
#' @return
#' A named list where each element is a data frame with two numeric
#' columns:
#' \itemize{
#'   \item \code{x}: raw measurement points
#'   \item \code{qi}: corresponding IQA sub-scores (0..100)
#' }
#'
#' @examples
#' # Internal helper: not exported. Example of inspecting the OD curve:
#' tbls <- iqa_curve_table()
#' head(tbls$od)
#'
#' @keywords internal
#' @noRd
#' @examples NULL
iqa_curve_table <- function(method = c("CETESB_approx")) {
  method <- match.arg(method)
  list(
    od = data.frame(
      x  = c(0, 2, 3, 4, 5, 6, 7, 8, 10, 14),
      qi = c(2,10,20,40,60,80,88,92,96,99)
    ),
    coliformes = data.frame(
      x  = c(1, 200, 1000, 10000, 100000),
      qi = c(100,90,70,40,10)
    ),
    pH = data.frame(
      x  = c(2, 5, 6, 7, 8.5, 9, 11),
      qi = c(5,40,80,90,90,80,20)
    ),
    dbo = data.frame(
      x  = c(0, 2, 3, 5, 8, 10, 15),
      qi = c(100,90,80,60,40,30,10)
    ),
    nt_total = data.frame(
      x  = c(0.1, 1, 3, 5, 10, 20, 50),
      qi = c(95,90,80,70,60,40,10)
    ),
    p_total = data.frame(
      x  = c(0.005, 0.05, 0.1, 0.2, 0.5, 1),
      qi = c(95,90,80,65,40,20)
    ),
    turbidez = data.frame(
      x  = c(0.5, 1, 5, 10, 50, 100, 200),
      qi = c(98,95,90,80,60,40,20)
    ),
    tds = data.frame(
      x  = c(50, 100, 250, 500, 1000, 1500),
      qi = c(95,92,88,80,60,40)
    ),
    temperatura = data.frame(
      x  = c(0, 2, 5, 10, 15),
      qi = c(100,90,75,55,40)
    )
  )
}

#' Piecewise-linear interpolation of Qi given a (x, qi) table
#'
#' @description
#' Computes interpolated sub-scores for a numeric vector \code{x} using
#' a two-column table with \code{x} and \code{qi}. Extrapolation at
#' the tails uses the end segments (rule = 2).
#'
#' @param x   Numeric vector of raw measurements.
#' @param tbl Data frame with numeric columns \code{x} and \code{qi}.
#'
#' @return Numeric vector of interpolated \code{qi} values (same length as \code{x}).
#'
#' @examples
#' # Internal helper: not exported.
#' tbl <- iqa_curve_table()$od
#' qi_interp(c(4, 6, 9), tbl)
#'
#' @keywords internal
#' @noRd
#' @examples NULL
qi_interp <- function(x, tbl) {
  stopifnot(is.numeric(x))
  stopifnot(is.data.frame(tbl), all(c("x","qi") %in% names(tbl)))
  stats::approx(x = tbl$x, y = tbl$qi, xout = x, rule = 2, ties = "ordered")$y
}

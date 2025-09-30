# R/viz.R
# ASCII-only in code and roxygen.

#' Time series by parameter
#'
#' @description Plot a time series for one numeric parameter, optionally
#'   colored/faceted by a grouping column.
#'
#' @param df Data frame with a `data` column (Date/POSIXct) and the parameter column.
#' @param parametro Character; name of the numeric column to plot on Y.
#' @param facet Character or `NULL`; optional grouping column name to color/facet.
#'
#' @returns A \code{ggplot} object.
#'
#' @seealso \code{\link[=plot_box]{plot_box()}}, \code{\link[=plot_heatmap]{plot_heatmap()}}, \code{\link[=iqa]{iqa()}}
#'
#' @examples
#' \donttest{
#' data(wq_demo)
#' # Basic: time series of turbidity
#' p <- plot_series(wq_demo, "turbidez")
#' # With color/facet by sampling point
#' p2 <- plot_series(wq_demo, "turbidez", facet = "ponto")
#' }
#'
#' @importFrom rlang sym .data
#' @export
plot_series <- function(df, parametro, facet = NULL) {
  if (!("data" %in% names(df))) stop("Column 'data' not found.")
  if (!(parametro %in% names(df))) stop("Column '", parametro, "' not found.")

  y_sym <- rlang::sym(parametro)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$data, y = !!y_sym))

  if (!is.null(facet) && facet %in% names(df)) {
    f_sym <- rlang::sym(facet)
    p <- p +
      ggplot2::geom_line(ggplot2::aes(color = !!f_sym)) +
      ggplot2::geom_point(ggplot2::aes(color = !!f_sym)) +
      ggplot2::facet_wrap(ggplot2::vars(!!f_sym))
  } else {
    p <- p +
      ggplot2::geom_line() +
      ggplot2::geom_point()
  }

  p + ggplot2::labs(x = "Date", y = parametro) + ggplot2::theme_minimal()
}

#' Boxplots by site/parameter
#'
#' @description Boxplots of one numeric parameter grouped by a categorical column.
#'
#' @param df Data frame with water quality data.
#' @param parametro Character; name of the numeric parameter column.
#' @param by Character; grouping column (e.g., "ponto").
#'
#' @returns A \code{ggplot} object.
#'
#' @seealso \code{\link[=plot_series]{plot_series()}}, \code{\link[=plot_heatmap]{plot_heatmap()}}, \code{\link[=iqa]{iqa()}}
#'
#' @examples
#' \donttest{
#' data(wq_demo)
#' plot_box(wq_demo, "turbidez", by = "ponto")
#' }
#'
#' @importFrom rlang sym
#' @export
plot_box <- function(df, parametro, by = "ponto") {
  if (!(parametro %in% names(df))) stop("Column '", parametro, "' not found.")
  if (!(by %in% names(df)))       stop("Column '", by, "' not found.")

  y_sym <- rlang::sym(parametro)
  x_sym <- rlang::sym(by)

  ggplot2::ggplot(df, ggplot2::aes(x = !!x_sym, y = !!y_sym)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = by, y = parametro) +
    ggplot2::theme_minimal()
}

#' Plot IQA by site/date
#'
#' @description Bar plot of IQA values per site/date. Requires an `IQA` column.
#'
#' @param df Data frame returned by `iqa()` (or with equivalent columns).
#'
#' @returns A \code{ggplot} object.
#'
#' @seealso \code{\link[=iqa]{iqa()}}, \code{\link[=plot_series]{plot_series()}}, \code{\link[=plot_box]{plot_box()}}
#'
#' @examples
#' \donttest{
#' data(wq_demo)
#' d <- iqa(wq_demo, na_rm = TRUE)
#' plot_iqa(d)
#' }
#'
#' @importFrom rlang .data
#' @export
plot_iqa <- function(df) {
  if (!("IQA" %in% names(df))) stop("Compute iqa() first: 'IQA' column not found.")
  if (!("ponto" %in% names(df))) stop("Column 'ponto' not found.")

  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = .data$ponto,
      y = .data$IQA,
      fill = cut(.data$IQA, c(0, 25, 50, 70, 90, 100), include.lowest = TRUE)
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::labs(x = "Ponto", y = "IQA", fill = "Faixa") +
    ggplot2::theme_minimal()
}

#' Heatmap of parameters vs. sites
#'
#' @description Heatmap for long-format data (date x parameter).
#'
#' @param df_long Long-format data frame with columns `data`, `parametro`, `valor`.
#'
#' @returns A \code{ggplot} object.
#'
#' @examples
#' \donttest{
#' # Example: reshape wq_demo to long and plot
#' data(wq_demo)
#' library(tidyr)
#' df_long <- tidyr::pivot_longer(
#'   wq_demo,
#'   cols = c("ph","od","turbidez","dbo"),
#'   names_to = "parametro",
#'   values_to = "valor"
#' )
#' plot_heatmap(df_long)
#' }
#'
#' @importFrom rlang .data
#' @export
plot_heatmap <- function(df_long){
  ggplot2::ggplot(
    df_long,
    ggplot2::aes(x = .data$data, y = .data$parametro, fill = .data$valor)
  ) +
    ggplot2::geom_tile() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Date", y = "Parameter", fill = "Value")
}
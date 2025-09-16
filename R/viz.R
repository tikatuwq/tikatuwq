#' Time series by parameter
#'
#' @param df Data frame with a \code{data} column (Date/POSIXct) and the chosen parameter column.
#' @param parametro Character; parameter to plot (name of the numeric column).
#' @param facet Character or \code{NULL}; optional grouping column name to facet/color.
#'
#' @return A ggplot object (class \code{ggplot}) showing the time series.
#'
#' @export

plot_series <- function(df, parametro, facet = NULL){
  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "data", y = parametro, color = facet)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::labs(x="Date", y=parametro) + ggplot2::theme_minimal()
  if(!is.null(facet) && facet %in% names(df)){
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet)))
  }
  p
}

#' Boxplots by site/parameter
#'
#' @param df Data frame with water quality data.
#' @param parametro Character; parameter column name.
#' @param by Character; grouping column (e.g., "site").
#'
#' @return A ggplot object (class \code{ggplot}) representing the boxplot.
#'
#' @export
plot_box <- function(df, parametro, by = "ponto"){
  ggplot2::ggplot(df, ggplot2::aes_string(x = by, y = parametro)) +
    ggplot2::geom_boxplot() + ggplot2::theme_minimal() +
    ggplot2::labs(x = by, y = parametro)
}

#' Plot IQA by site/date
#'
#' @param df Data frame returned by \code{iqa()} (or with equivalent columns).
#'
#' @return A ggplot object (class \code{ggplot}) representing IQA across sites/dates.
#'
#' @export
plot_iqa <- function(df){
  if(!"IQA" %in% names(df)) stop("Compute iqa() first.")
  ggplot2::ggplot(df, ggplot2::aes(x = ponto, y = IQA, fill = cut(IQA, c(0,25,50,70,90,100), include.lowest = TRUE))) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(limits = c(0,100)) +
    ggplot2::labs(x="Ponto", y="IQA", fill="Faixa") +
    ggplot2::theme_minimal()
}

#' Heatmap of parameters vs. sites
#'
#' @param df_long Long-format data frame (site/parameter/value).
#'
#' @return A ggplot object (class \code{ggplot}) representing the heatmap.
#'
#' @export
plot_heatmap <- function(df_long){
  ggplot2::ggplot(df_long, ggplot2::aes(x = data, y = parametro, fill = valor)) +
    ggplot2::geom_tile() + ggplot2::theme_minimal() +
    ggplot2::labs(x="Date", y="Parameter", fill="Value")
}

#' Séries temporais por parâmetro
#' @export
plot_series <- function(df, parametro, facet = NULL){
  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "data", y = parametro, color = facet)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::labs(x="Data", y=parametro) + ggplot2::theme_minimal()
  if(!is.null(facet) && facet %in% names(df)){
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet)))
  }
  p
}

#' Boxplot por grupo
#' @export
plot_box <- function(df, parametro, by = "ponto"){
  ggplot2::ggplot(df, ggplot2::aes_string(x = by, y = parametro)) +
    ggplot2::geom_boxplot() + ggplot2::theme_minimal() +
    ggplot2::labs(x = by, y = parametro)
}

#' Barras de IQA por ponto
#' @export
plot_iqa <- function(df){
  if(!"IQA" %in% names(df)) stop("Calcule iqa() antes.")
  ggplot2::ggplot(df, ggplot2::aes(x = ponto, y = IQA, fill = cut(IQA, c(0,25,50,70,90,100), include.lowest = TRUE))) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(limits = c(0,100)) +
    ggplot2::labs(x="Ponto", y="IQA", fill="Faixa") +
    ggplot2::theme_minimal()
}

#' Heatmap tempo x parâmetro (formato longo)
#' @export
plot_heatmap <- function(df_long){
  ggplot2::ggplot(df_long, ggplot2::aes(x = data, y = parametro, fill = valor)) +
    ggplot2::geom_tile() + ggplot2::theme_minimal() +
    ggplot2::labs(x="Data", y="Parâmetro", fill="Valor")
}

#' Multi-parameter analysis wrappers
#'
#' Wrappers para operar com **varios parametros** por chamada, reaproveitando
#' a API base (1 parametro): `param_summary()`, `param_trend()`, `param_plot()`.
#'
#' @keywords internal
#' @name param_analysis_multi
NULL


# ---------------------------- Resumos multi-param -------------------------------

#' Resumo para varios parametros (filtro por rio/ponto)
#'
#' @description
#' Itera sobre um vetor de `parametros`, chamando `param_summary()` para cada um,
#' e combina as saidas em uma unica tabela, acrescentando a coluna `parametro`.
#'
#' @param df Data frame com colunas necessarias (ver `param_summary()`).
#' @param parametros Vetor de nomes de parametros (ex.: c("turbidez","od","pH")).
#' @param rios Vetor de rios para filtrar (opcional; usa coluna `rio` se existir).
#' @param pontos Vetor de pontos para filtrar (opcional; usa coluna `ponto` se existir).
#' @param period "none","month","quarter","year" (igual a `param_summary()`).
#' @param na_rm Logical; repassado para `param_summary()`.
#'
#' @return Tibble combinando os resumos de todos os parametros,
#' com coluna `parametro` indicando a origem.
#' @examples
#' \dontrun{
#' data("wq_demo", package = "tikatuwq")
#' param_summary_multi(wq_demo, c("turbidez","od"), pontos = c("P1","P2"))
#' }
#' @export
#' @family parameter-tools
param_summary_multi <- function(df, parametros,
                                rios = NULL, pontos = NULL,
                                period = c("none","month","quarter","year"),
                                na_rm = TRUE) {
  period <- match.arg(period)
  params_ok <- intersect(parametros, names(df))
  if (length(params_ok) == 0) {
    stop("Nenhum dos parametros especificados foi encontrado no data frame.", call. = FALSE)
  }
  out_list <- lapply(params_ok, function(p){
    res <- param_summary(df, parametro = p,
                         rios = rios, pontos = pontos,
                         period = period, na_rm = na_rm)
    if (!tibble::is_tibble(res)) res <- tibble::as_tibble(res)
    res$parametro <- p
    res
  })
  dplyr::bind_rows(out_list)
}

# ---------------------------- Tendencias multi-param ----------------------------

#' Tendencia para varios parametros (filtro por rio/ponto)
#'
#' @description
#' Itera sobre um vetor de `parametros`, chamando `param_trend()` para cada um,
#' e combina as saidas em uma unica tabela, acrescentando a coluna `parametro`.
#'
#' @param df Data frame com `data` e colunas dos parametros.
#' @param parametros Vetor de nomes de parametros.
#' @param rios Vetor de rios (opcional; usa coluna `rio` se existir).
#' @param pontos Vetor de pontos (opcional; usa coluna `ponto` se existir).
#' @param na_rm Logical; repassado para `param_trend()`.
#'
#' @return Tibble combinando as tendencias de todos os parametros,
#' com coluna `parametro`.
#' @examples
#' \dontrun{
#' data("wq_demo", package = "tikatuwq")
#' param_trend_multi(wq_demo, c("turbidez","od"), pontos = "P1")
#' }
#' @export
#' @family parameter-tools
param_trend_multi <- function(df, parametros,
                              rios = NULL, pontos = NULL,
                              na_rm = TRUE) {
  if (!"data" %in% names(df)) {
    stop("A coluna 'data' eh obrigatoria para calcular tendencias.", call. = FALSE)
  }
  params_ok <- intersect(parametros, names(df))
  if (length(params_ok) == 0) {
    stop("Nenhum dos parametros especificados foi encontrado no data frame.", call. = FALSE)
  }
  out_list <- lapply(params_ok, function(p){
    res <- param_trend(df, parametro = p, rios = rios, pontos = pontos, na_rm = na_rm)
    if (!tibble::is_tibble(res)) res <- tibble::as_tibble(res)
    res$parametro <- p
    res
  })
  dplyr::bind_rows(out_list)
}

# ------------------------------- Plots multi-param ------------------------------

#' Plot temporal para varios parametros (filtro por rio/ponto)
#'
#' @description
#' Combina varios parametros em um unico grafico. Por padrao:
#' - cor = `ponto` (se existir);
#' - `facet = "parametro"` cria paineis por parametro;
#' - `facet = "grid"` usa grade `ponto ~ parametro` quando ha mais de um ponto.
#'
#' @param df Data frame com `data` e colunas dos parametros.
#' @param parametros Vetor de nomes de parametros.
#' @param rios Vetor de rios para filtrar (opcional; usa coluna `rio` se existir).
#' @param pontos Vetor de pontos para filtrar (opcional; usa coluna `ponto` se existir).
#' @param add_trend Logical; se TRUE, adiciona reta `lm` em cada painel.
#' @param facet "parametro", "none" ou "grid".
#'
#' @return Objeto `ggplot`.
#' @examples
#' \dontrun{
#' data("wq_demo", package = "tikatuwq")
#' param_plot_multi(wq_demo, c("turbidez","od"), pontos = c("P1","P2"),
#'                  add_trend = TRUE, facet = "grid")
#' }
#' @export
#' @family parameter-tools
param_plot_multi <- function(df, parametros,
                             rios = NULL, pontos = NULL,
                             add_trend = TRUE,
                             facet = c("parametro","none","grid")) {
  facet <- match.arg(facet)
  if (!"data" %in% names(df)) {
    stop("A coluna 'data' eh obrigatoria para plot temporal.", call. = FALSE)
  }
  params_ok <- intersect(parametros, names(df))
  if (length(params_ok) == 0) {
    stop("Nenhum dos parametros especificados foi encontrado no data frame.", call. = FALSE)
  }

  d <- .filter_scope(df, rios = rios, pontos = pontos)
  d$data <- .as_date_safe(d$data)

  # formato longo: data, rio?, ponto?, parametro, valor
  keep_cols <- unique(c("data", intersect(c("rio","ponto"), names(d)), params_ok))
  d <- d[, keep_cols, drop = FALSE]

  long <- tidyr::pivot_longer(
    data = d,
    cols = dplyr::all_of(params_ok),
    names_to = "parametro",
    values_to = "valor"
  )

  has_ponto <- "ponto" %in% names(long)

  aes_base <- if (has_ponto) {
    ggplot2::aes(x = .data$data, y = .data$valor, color = .data$ponto)
  } else {
    ggplot2::aes(x = .data$data, y = .data$valor)
  }

  g <- ggplot2::ggplot(long, aes_base) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::labs(x = NULL, y = NULL)

  if (add_trend) {
    g <- g + ggplot2::geom_smooth(method = "lm", se = FALSE, na.rm = TRUE)
  }

  if (facet == "parametro") {
    g <- g + ggplot2::facet_wrap(~parametro, scales = "free_y")
  } else if (facet == "grid" && has_ponto) {
    g <- g + ggplot2::facet_grid(ponto ~ parametro, scales = "free_y")
  }

  g
}
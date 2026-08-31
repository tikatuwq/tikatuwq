# R/wq_pca.R
# Analise de Componentes Principais (PCA) para qualidade da agua
# (ASCII-only no codigo)

#' Analise de Componentes Principais (PCA) de parametros de qualidade da agua
#'
#' @description
#' Wrapper simplificado sobre \code{stats::prcomp()} para dados de qualidade
#' da agua. Retorna o objeto PCA, scores por amostra, contribuicao das
#' variaveis (loadings), variancia explicada e dois graficos prontos
#' (biplot e screeplot).
#'
#' @details
#' Apenas colunas numericas sao consideradas. Linhas com qualquer NA
#' nos parametros selecionados sao removidas (com aviso). O PCA e
#' sempre realizado sobre dados centrados e escalonados
#' (\code{scale. = TRUE, center = TRUE}).
#'
#' Os graficos sao retornados como atributos do resultado:
#' \itemize{
#'   \item \code{attr(resultado, "biplot")} -- dispersao das amostras nos
#'     eixos PC1 x PC2, coloridas por \code{color_by}.
#'   \item \code{attr(resultado, "screeplot")} -- variancia explicada
#'     por componente.
#'   \item \code{attr(resultado, "loadings_plot")} -- contribuicao das
#'     variaveis no plano PC1 x PC2.
#' }
#'
#' @param df Data frame com os parametros a incluir na analise.
#' @param params Character vector; nomes das colunas a usar. Se \code{NULL}
#'   (default), todas as colunas numericas sao usadas (excluindo \code{lat},
#'   \code{lon}, e colunas com sufixo \code{_ok}, \code{_status},
#'   \code{_delta}).
#' @param color_by Character ou \code{NULL}; coluna para colorir as amostras
#'   no biplot (ex.: \code{"ponto"}, \code{"season"}).
#' @param label_by Character ou \code{NULL}; coluna para rotular as amostras
#'   no biplot (ex.: \code{"ponto"}).
#' @param n_components Integer; numero de componentes a reter. Default \code{4}.
#'
#' @returns
#' Uma lista com:
#' \describe{
#'   \item{pca}{Objeto \code{prcomp}.}
#'   \item{scores}{Tibble com scores (PC1...PCn) por amostra, mais
#'     as colunas de agrupamento (\code{color_by}, \code{label_by}).}
#'   \item{loadings}{Tibble com loadings das variaveis.}
#'   \item{variance}{Tibble com variancia explicada e acumulada por componente.}
#' }
#' Os atributos \code{"biplot"}, \code{"screeplot"} e \code{"loadings_plot"}
#' contem objetos \code{ggplot}.
#'
#' @seealso \code{\link[=param_analysis]{param_analysis()}}
#'
#' @family multivariate-tools
#'
#' @importFrom stats prcomp complete.cases
#' @importFrom tibble as_tibble tibble
#' @importFrom rlang .data sym
#' @importFrom ggplot2 ggplot aes geom_point geom_text geom_segment
#'   geom_col scale_color_brewer labs theme_minimal theme
#'   element_text arrow unit
#'
#' @examples
#' \donttest{
#' data("wq_demo", package = "tikatuwq")
#' res <- wq_pca(wq_demo, color_by = "ponto")
#' print(res$variance)
#' attr(res, "biplot")
#' attr(res, "screeplot")
#' }
#'
#' @export
wq_pca <- function(
  df,
  params       = NULL,
  color_by     = NULL,
  label_by     = NULL,
  n_components = 4L
) {
  stopifnot(is.data.frame(df))
  n_components <- as.integer(n_components)

  # Selecao das colunas numericas
  exclude_patterns <- c("_ok$","_status$","_delta$","__lim_","IQA_","TSI_","IET_",
                        "^lat$","^lon$","^latitude$","^longitude$",
                        "^IQA$","^NSFWQI$","^IET$","^TSI$")
  all_num <- names(df)[vapply(df, is.numeric, logical(1))]

  if (is.null(params)) {
    drop <- unique(unlist(lapply(exclude_patterns, function(pat) {
      grep(pat, all_num, value = TRUE)
    })))
    params <- setdiff(all_num, drop)
  } else {
    missing_p <- setdiff(params, names(df))
    if (length(missing_p)) stop("Parametros nao encontrados: ", paste(missing_p, collapse = ", "))
    params <- intersect(params, all_num)
  }

  if (length(params) < 2) stop("PCA requer ao menos 2 parametros numericos.")

  # Remove NAs
  sub_df <- df[, params, drop = FALSE]
  rows_ok <- complete.cases(sub_df)
  if (any(!rows_ok)) {
    n_rem <- sum(!rows_ok)
    warning(n_rem, " linha(s) removida(s) por conter NA em um ou mais parametros.")
  }
  sub_df <- sub_df[rows_ok, , drop = FALSE]
  if (nrow(sub_df) < 3) stop("PCA requer ao menos 3 observacoes completas.")

  # PCA
  pca_obj <- stats::prcomp(sub_df, center = TRUE, scale. = TRUE)

  # Numero de componentes a reter
  n_keep <- min(n_components, ncol(pca_obj$rotation), nrow(sub_df) - 1L)
  scores_mat <- pca_obj$x[, seq_len(n_keep), drop = FALSE]

  # Variancia explicada
  var_exp <- (pca_obj$sdev^2) / sum(pca_obj$sdev^2)
  variance_tb <- tibble::tibble(
    componente    = paste0("PC", seq_along(pca_obj$sdev)),
    variancia_pct = round(var_exp * 100, 2),
    acumulada_pct = round(cumsum(var_exp) * 100, 2)
  )

  # Scores
  aux_cols <- unique(c(color_by, label_by))
  aux_cols <- aux_cols[!is.null(aux_cols) & aux_cols %in% names(df)]
  scores_tb <- tibble::as_tibble(as.data.frame(scores_mat))
  if (length(aux_cols)) {
    aux_df    <- df[rows_ok, aux_cols, drop = FALSE]
    scores_tb <- cbind(aux_df, scores_tb)
    scores_tb <- tibble::as_tibble(scores_tb)
  }

  # Loadings
  load_mat <- pca_obj$rotation[, seq_len(n_keep), drop = FALSE]
  loadings_tb <- tibble::tibble(
    variavel = rownames(load_mat),
    as.data.frame(load_mat)
  )

  # ---- Graficos -----------------------------------------------------------

  # Screeplot
  gg_scree <- ggplot2::ggplot(
    variance_tb[seq_len(min(10, nrow(variance_tb))), ],
    ggplot2::aes(x = .data$componente, y = .data$variancia_pct)
  ) +
    ggplot2::geom_col(fill = "#4C9BE8", alpha = 0.8) +
    ggplot2::geom_line(ggplot2::aes(group = 1), colour = "#333333") +
    ggplot2::geom_point(colour = "#333333", size = 2) +
    ggplot2::labs(
      x     = "Componente",
      y     = "Variancia explicada (%)",
      title = "Screeplot -- variancia por componente PCA"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # Biplot (PC1 x PC2)
  pc_labels <- paste0("PC", 1:2, " (", round(var_exp[1:2] * 100, 1), "%)")

  if (!is.null(color_by) && color_by %in% names(scores_tb)) {
    gg_bi <- ggplot2::ggplot(
      scores_tb,
      ggplot2::aes(x = .data$PC1, y = .data$PC2,
                   colour = !!rlang::sym(color_by))
    )
  } else {
    gg_bi <- ggplot2::ggplot(scores_tb, ggplot2::aes(x = .data$PC1, y = .data$PC2))
  }

  gg_bi <- gg_bi +
    ggplot2::geom_point(size = 2.5, alpha = 0.8) +
    ggplot2::labs(
      x      = pc_labels[1],
      y      = pc_labels[2],
      colour = color_by,
      title  = "Biplot PCA -- amostras"
    ) +
    ggplot2::theme_minimal()

  if (!is.null(label_by) && label_by %in% names(scores_tb)) {
    gg_bi <- gg_bi +
      ggplot2::geom_text(ggplot2::aes(label = !!rlang::sym(label_by)),
                         size = 3, hjust = 1.2, alpha = 0.7)
  }

  # Loadings plot (PC1 x PC2)
  scale_arrow <- max(abs(scores_mat[, 1:2])) / max(abs(load_mat[, 1:2])) * 0.7
  load_df <- data.frame(
    variavel = rownames(load_mat),
    x_end    = load_mat[, 1] * scale_arrow,
    y_end    = load_mat[, 2] * scale_arrow
  )
  gg_load <- ggplot2::ggplot(load_df, ggplot2::aes(x = 0, y = 0,
                                                    xend = .data$x_end,
                                                    yend = .data$y_end)) +
    ggplot2::geom_segment(
      arrow  = ggplot2::arrow(length = ggplot2::unit(0.3, "cm")),
      colour = "#D53E4F", linewidth = 0.7
    ) +
    ggplot2::geom_text(ggplot2::aes(x = .data$x_end * 1.08,
                                    y = .data$y_end * 1.08,
                                    label = .data$variavel),
                       size = 3.5, colour = "#333333") +
    ggplot2::labs(
      x     = pc_labels[1],
      y     = pc_labels[2],
      title = "Contribuicao das variaveis -- PC1 x PC2"
    ) +
    ggplot2::theme_minimal()

  # ---- Resultado final ----------------------------------------------------
  result <- list(
    pca      = pca_obj,
    scores   = scores_tb,
    loadings = loadings_tb,
    variance = variance_tb
  )
  attr(result, "biplot")        <- gg_bi
  attr(result, "screeplot")     <- gg_scree
  attr(result, "loadings_plot") <- gg_load
  class(result) <- c("wq_pca", "list")
  result
}

#' @export
print.wq_pca <- function(x, ...) {
  cat("=== PCA de qualidade da agua ===\n")
  cat("Parametros analisados:", ncol(x$pca$rotation), "\n")
  cat("Amostras:             ", nrow(x$scores), "\n\n")
  cat("Variancia explicada:\n")
  print(x$variance[x$variance$acumulada_pct <= 95 | seq_len(nrow(x$variance)) <= 3, ])
  cat("\nUse attr(resultado, 'biplot') para o grafico de dispersao.\n")
  cat("Use attr(resultado, 'screeplot') para o screeplot.\n")
  invisible(x)
}

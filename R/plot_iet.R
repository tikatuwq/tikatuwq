# R/plot_iet.R
# Visualizacao do Indice de Estado Trofico (IET / TSI)
# (ASCII-only no codigo)

#' Visualiza o Indice de Estado Trofico (IET / TSI)
#'
#' @description
#' Grafico de barras horizontais ou colunas verticais para o IET/TSI,
#' com coloracao por classe trofica. Aceita resultados das funcoes
#' \code{iet_carlson()} ou \code{iet_lamparelli()}.
#'
#' @details
#' A funcao detecta automaticamente a coluna de IET: procura por
#' \code{"IET"}, \code{"TSI"}, \code{"IET_Carlson"} ou \code{"IET_Lamparelli"}.
#' Tambem e possivel especificar o nome via \code{iet_col}.
#'
#' As faixas de classificacao trofica seguem o metodo escolhido:
#' \itemize{
#'   \item Carlson (1977): Ultraoligo (<30), Oligo (30-40), Meso (40-50),
#'     Eutro (50-70), Hipereutro (>70).
#'   \item Lamparelli (2004): Ultraoligo (<47), Oligo (47-52), Meso (52-59),
#'     Eutro (59-63), Supereutro (63-67), Hipereutro (>67).
#' }
#'
#' @param df Data frame retornado por \code{iet_carlson()} ou
#'   \code{iet_lamparelli()}, ou qualquer data frame com uma coluna de
#'   IET numerica e a coluna \code{ponto}.
#' @param iet_col Character; nome da coluna de IET.
#'   Se \code{NULL} (default), detectada automaticamente.
#' @param method Character; metodo de classificacao trofica:
#'   \code{"carlson"} (default) ou \code{"lamparelli"}.
#' @param orientation Character; \code{"vertical"} (default) ou
#'   \code{"horizontal"}.
#' @param facet Character ou \code{NULL}; coluna para facetar
#'   (ex.: \code{"rio"}). Default \code{NULL}.
#'
#' @returns Um objeto \code{ggplot}.
#'
#' @seealso \code{\link[=iet_carlson]{iet_carlson()}},
#'   \code{\link[=iet_lamparelli]{iet_lamparelli()}},
#'   \code{\link[=plot_iqa]{plot_iqa()}}
#'
#' @family visualization-tools
#'
#' @examples
#' \donttest{
#' data("wq_demo", package = "tikatuwq")
#' df_iet <- iet_carlson(wq_demo, .keep_ids = TRUE)
#' plot_iet(df_iet, method = "carlson")
#'
#' df_lamp <- iet_lamparelli(wq_demo, ambiente = "rio", .keep_ids = TRUE)
#' plot_iet(df_lamp, method = "lamparelli")
#' }
#'
#' @importFrom rlang .data sym
#' @importFrom ggplot2 ggplot aes geom_col geom_hline scale_fill_manual
#'   scale_y_continuous coord_flip labs theme_minimal theme
#'   facet_wrap vars
#' @export
plot_iet <- function(
  df,
  iet_col     = NULL,
  method      = c("carlson", "lamparelli"),
  orientation = c("vertical", "horizontal"),
  facet       = NULL
) {
  method      <- match.arg(method)
  orientation <- match.arg(orientation)
  stopifnot(is.data.frame(df))

  # Detecta coluna de IET
  if (is.null(iet_col)) {
    candidates <- c("IET", "TSI", "IET_Carlson", "IET_Lamparelli", "IET_Lamp", "iet", "tsi")
    iet_col    <- candidates[candidates %in% names(df)][1]
    if (is.na(iet_col)) {
      stop("Coluna de IET nao detectada. Fornecer 'iet_col' explicitamente ",
           "ou computar com iet_carlson() / iet_lamparelli() primeiro.")
    }
  }
  if (!iet_col %in% names(df)) stop("Coluna '", iet_col, "' nao encontrada.")
  if (!"ponto" %in% names(df)) stop("Coluna 'ponto' nao encontrada.")

  # Faixas e cores por metodo
  if (method == "carlson") {
    breaks <- c(-Inf, 30, 40, 50, 70, Inf)
    labels <- c("Ultraoligotrofico","Oligotrofico","Mesotrofico",
                "Eutrofico","Hipereutrofico")
    cores  <- c("Ultraoligotrofico" = "#3288BD",
                "Oligotrofico"      = "#66C2A5",
                "Mesotrofico"       = "#FEE08B",
                "Eutrofico"         = "#FC8D59",
                "Hipereutrofico"    = "#D53E4F")
    ref_lines <- c(30, 40, 50, 70)
  } else {
    breaks <- c(-Inf, 47, 52, 59, 63, 67, Inf)
    labels <- c("Ultraoligotrofico","Oligotrofico","Mesotrofico",
                "Eutrofico","Supereutrofico","Hipereutrofico")
    cores  <- c("Ultraoligotrofico" = "#3288BD",
                "Oligotrofico"      = "#66C2A5",
                "Mesotrofico"       = "#FEE08B",
                "Eutrofico"         = "#FC8D59",
                "Supereutrofico"    = "#F46D43",
                "Hipereutrofico"    = "#D53E4F")
    ref_lines <- c(47, 52, 59, 63, 67)
  }

  iet_vals   <- suppressWarnings(as.numeric(df[[iet_col]]))
  df[[".iet."]]  <- iet_vals
  df[[".classe."]] <- as.character(cut(iet_vals, breaks = breaks,
                                       labels = labels, right = TRUE))
  df[[".classe."]][is.na(df[[".classe."]])] <- "Nao classificado"

  iet_sym    <- rlang::sym(".iet.")
  classe_sym <- rlang::sym(".classe.")
  ponto_sym  <- rlang::sym("ponto")

  p <- ggplot2::ggplot(
    df[!is.na(iet_vals), , drop = FALSE],
    ggplot2::aes(
      x    = !!ponto_sym,
      y    = !!iet_sym,
      fill = !!classe_sym
    )
  ) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::scale_fill_manual(
      values   = cores,
      na.value = "grey70",
      name     = "Classe trofica"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, .08))) +
    ggplot2::labs(
      x        = "Ponto",
      y        = paste0("IET (", tools::toTitleCase(method), ")"),
      title    = paste0("Indice de Estado Trofico \u2014 ", tools::toTitleCase(method)),
      subtitle = paste0("Enquadramento por faixas: ", paste(ref_lines, collapse = " | "))
    ) +
    ggplot2::geom_hline(yintercept = ref_lines, linetype = "dashed",
                        colour = "grey40", linewidth = 0.4) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1)
    )

  if (orientation == "horizontal") p <- p + ggplot2::coord_flip()

  if (!is.null(facet) && facet %in% names(df)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(facet)))
  }

  p
}

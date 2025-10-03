# Register columns computed on-the-fly to appease R CMD check
utils::globalVariables(c(".x", ".y", ".key"))

#' Linha de tendencia temporal para parametros de qualidade da agua
#'
#' Gera um grafico de series temporais com pontos observados e linhas de
#' tendencia ajustadas. Suporta metodos robustos (Theil-Sen), lineares (OLS)
#' ou suavizados (LOESS). Util para verificar tendencias de parametros
#' ambientais por ponto e/ou rio.
#'
#' @param data data.frame. Deve conter ao menos uma coluna de datas e a coluna
#'   do parametro a ser analisado.
#' @param param character. Nome da coluna do parametro (ex.: "turbidez", "iqa").
#' @param date_col character. Nome da coluna de datas. Default = "data".
#' @param group_cols character. Vetor com colunas para agrupamento (ex.: c("rio","ponto")).
#'   Use "none" para nao facetar. Default = c("rio","ponto").
#' @param method character. Metodo de ajuste da tendencia:
#'   - "theilsen" (padrao): regressao Theil-Sen (robusta a outliers).
#'   - "ols": regressao linear simples (minimos quadrados).
#'   - "loess": curva suavizada, sem inclinacao unica.
#' @param show_points logical. Mostrar pontos observados? Default = TRUE.
#' @param min_n integer. Numero minimo de observacoes por grupo para calcular
#'   tendencia. Default = 6.
#'
#' @return Objeto ggplot2, que pode ser plotado diretamente.
#' @export
#'
#' @details
#' - A funcao desenha pontos e linhas conectando as observacoes, alem da linha
#'   de tendencia calculada pelo metodo escolhido.
#' - Quando group_cols possui mais de uma categoria, os grupos sao facetados.
#' - "theilsen" e mais robusto a valores atipicos do que "ols".
#' - "loess" e util quando nao se espera relacao linear no tempo.
#'
#' @seealso [plot_series()], [iqa()]
#'
#' @importFrom stats coef loess loess.control median cor.test lm predict as.formula
#' @importFrom utils tail globalVariables
#'
#' @examples
#' # Exemplo simples: turbidez com tendencia Theil-Sen
#' set.seed(1)
#' df <- data.frame(
#'   data = as.Date("2024-01-01") + 0:11*30,
#'   rio = "Demo", ponto = "P1",
#'   turbidez = 20 + (-0.3)*(0:11) + rnorm(12, 0, 1)
#' )
#' plot_trend(df, param = "turbidez", method = "theilsen")
#'
#' # Exemplo com multiplos grupos e facetamento (OLS)
#' df2 <- data.frame(
#'   data = rep(seq(as.Date("2024-01-01"), by = "30 days", length.out = 12), 2),
#'   rio = rep(c("Rio A","Rio B"), each = 12),
#'   ponto = rep(c("P1","P2"), each = 12),
#'   od = c(7 + rnorm(12, 0, 0.5), 6 + rnorm(12, 0, 0.5))
#' )
#' plot_trend(df2, param = "od", method = "ols")
plot_trend <- function(data,
                       param,
                       date_col = "data",
                       group_cols = c("rio","ponto"),
                       method = c("theilsen","ols","loess"),
                       show_points = TRUE,
                       min_n = 6) {

  stopifnot(is.data.frame(data))
  if (!date_col %in% names(data)) stop("Coluna de data nao encontrada: ", date_col)
  if (!param %in% names(data))    stop("Parametro nao encontrado: ", param)

  method <- match.arg(method)

  df <- data
  df[[date_col]] <- as.Date(df[[date_col]])
  # y convertida, mas mantemos original no data.frame para a estetica y = param
  y_num <- suppressWarnings(as.numeric(df[[param]])); rm(y_num)

  # grupos: manter apenas os que existem
  if (identical(group_cols, "none")) {
    df$.__grp__ <- "all"
    group_cols <- ".__grp__"
  } else {
    group_cols <- group_cols[group_cols %in% names(df)]
    if (!length(group_cols)) {
      df$.__grp__ <- "all"
      group_cols <- ".__grp__"
    }
  }

  # helper: theil-sen (mesmo de trend_param)
  sen_fit <- function(tt, yy) {
    n <- length(yy)
    if (n < 2) return(list(slope = NA_real_, intercept = NA_real_))
    k <- 0L
    slopes <- numeric(n * (n - 1L) / 2L)
    for (i in 1:(n - 1L)) {
      dt <- tt[(i + 1L):n] - tt[i]
      dy <- yy[(i + 1L):n] - yy[i]
      ok <- is.finite(dt) & dt != 0 & is.finite(dy)
      if (any(ok)) {
        s <- dy[ok] / dt[ok]
        m <- length(s)
        slopes[(k + 1L):(k + m)] <- s
        k <- k + m
      }
    }
    if (k == 0L) return(list(slope = NA_real_, intercept = NA_real_))
    slopes <- slopes[seq_len(k)]
    slope <- stats::median(slopes, na.rm = TRUE)
    intercept <- stats::median(yy - slope * tt, na.rm = TRUE)
    list(slope = slope, intercept = intercept)
  }

  # preparar dados por grupo
  key <- interaction(df[, group_cols, drop = FALSE], drop = TRUE)
  df$.__key__ <- key

  # base do plot
  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = date_col, y = param, group = ".__key__"))

  if (show_points) {
    p <- p + ggplot2::geom_point(alpha = 0.85)
  }
  p <- p + ggplot2::geom_line(alpha = 0.6)

  # adicionar tendencia por grupo
  add_by_group <- function(d) {
    d <- d[order(d[[date_col]]), , drop = FALSE]
    x <- d[[date_col]]
    y <- suppressWarnings(as.numeric(d[[param]]))
    ok <- !is.na(x) & is.finite(y)
    d <- d[ok, , drop = FALSE]
    x <- x[ok]; y <- y[ok]
    if (length(y) < min_n) return(NULL)

    if (method %in% c("theilsen","ols")) {
      # reta entre min e max (no dominio observado)
      t_years <- as.numeric(x - min(x)) / 365.25
      if (method == "theilsen") {
        fit <- sen_fit(t_years, y)
        slope <- fit$slope; intercept <- fit$intercept
      } else { # ols
        fit <- stats::lm(y ~ t_years)
        slope <- unname(stats::coef(fit)[["t_years"]]); intercept <- unname(stats::coef(fit)[["(Intercept)"]])
      }
      t_line <- seq(min(t_years), max(t_years), length.out = 100)
      x_line <- min(x) + as.integer(t_line * 365.25)
      y_line <- intercept + slope * t_line
      data.frame(.x = as.Date(x_line, origin = "1970-01-01"),
                 .y = y_line,
                 .key = unique(d$.__key__))
    } else {
      # loess: curva suave
      df_fit <- data.frame(y = y, t = as.numeric(x))
      fit <- try(stats::loess(y ~ t, data = df_fit, control = stats::loess.control(surface = "direct")), silent = TRUE)
      if (inherits(fit, "try-error")) return(NULL)
      xs <- seq(min(x), max(x), by = "1 day")
      ys <- stats::predict(fit, newdata = data.frame(t = as.numeric(xs)))
      data.frame(.x = xs, .y = ys, .key = unique(d$.__key__))
    }
  }

  pieces <- split(df, df$.__key__)
  lines <- lapply(pieces, add_by_group)
  lines <- lines[!vapply(lines, is.null, logical(1))]
  if (length(lines)) {
    lines <- do.call(rbind, lines)
    p <- p + ggplot2::geom_line(
      data = lines,
      ggplot2::aes(x = .x, y = .y, group = .key),
      linewidth = 1.1
    )
  }

  # facetas (se houver mais de um grupo)
  if (!identical(group_cols, ".__grp__")) {
    # usa a ultima coluna de group_cols como faceta por padrao
    facet_col <- utils::tail(group_cols, 1)
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_col)), scales = "free_y")
  }

  p +
    ggplot2::labs(x = NULL, y = param, color = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

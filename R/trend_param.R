#' Tendencia monotona por parametro e ponto (Theil-Sen + Spearman)
#'
#' Calcula a inclinacao de Theil-Sen (robusta) e o p-valor do teste
#' de correlacao de Spearman entre tempo e o valor do parametro.
#' Retorna estatisticas por grupo (ex.: rio, ponto).
#'
#' @param data data.frame com pelo menos uma coluna de data e a coluna do parametro.
#' @param param nome do parametro (string), por exemplo "turbidez" ou "iqa".
#' @param date_col nome da coluna de datas. Default: "data".
#' @param group_cols vetor de nomes para agrupar. Default: c("rio","ponto").
#' @param min_n amostra minima por grupo. Default: 6.
#' @param alpha nivel de significancia para classificar tendencia. Default: 0.05.
#'
#' @return data.frame com colunas por grupo e:
#'   n, date_min, date_max, days_span, slope_per_year, intercept,
#'   rho_spearman, p_value, trend ("aumento" / "queda" / "estavel"),
#'   pct_change_period (aprox. % no periodo observado).
#' @export
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   data = as.Date("2024-01-01") + 0:11*30,
#'   rio = "Demo", ponto = "P1",
#'   turbidez = 20 + (-0.3)*(0:11) + rnorm(12, 0, 1)
#' )
#' trend_param(df, param = "turbidez")
trend_param <- function(data,
                        param,
                        date_col = "data",
                        group_cols = c("rio","ponto"),
                        min_n = 6,
                        alpha = 0.05) {

  stopifnot(is.data.frame(data))
  if (!date_col %in% names(data)) stop("Coluna de data nao encontrada: ", date_col)
  if (!param %in% names(data))    stop("Parametro nao encontrado: ", param)

  df <- data
  # coercao de datas
  df[[date_col]] <- as.Date(df[[date_col]])
  if (any(is.na(df[[date_col]]))) {
    # permitir NA, mas remover depois por grupo
    invisible()
  }
  # series/grupos: usar apenas os que existem
  group_cols <- group_cols[group_cols %in% names(df)]
  if (!length(group_cols)) {
    df$.__grp__ <- "all"
    group_cols <- ".__grp__"
  }

  # util: ajuste Theil-Sen em O(n^2)
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

  # split por grupo
  key <- interaction(df[, group_cols, drop = FALSE], drop = TRUE)
  pieces <- split(seq_len(nrow(df)), key)
  out <- vector("list", length(pieces))
  j <- 0L

  for (nm in names(pieces)) {
    idx <- pieces[[nm]]
    g <- df[idx, , drop = FALSE]
    # ordenar e limpar
    o <- order(g[[date_col]])
    g <- g[o, , drop = FALSE]
    x_date <- g[[date_col]]
    y <- suppressWarnings(as.numeric(g[[param]]))
    ok <- !is.na(x_date) & is.finite(y)
    g <- g[ok, , drop = FALSE]
    x_date <- x_date[ok]
    y <- y[ok]
    n <- length(y)

    if (n < min_n) {
      res <- data.frame(
        n = n,
        date_min = if (n) min(x_date) else as.Date(NA),
        date_max = if (n) max(x_date) else as.Date(NA),
        days_span = if (n) as.numeric(diff(range(x_date))) else NA_real_,
        slope_per_year = NA_real_,
        intercept = NA_real_,
        rho_spearman = NA_real_,
        p_value = NA_real_,
        trend = "indefinido",
        pct_change_period = NA_real_
      )
    } else {
      # tempo em anos desde a primeira observacao
      t_years <- as.numeric(x_date - min(x_date)) / 365.25
      fit <- sen_fit(t_years, y)
      slope_py <- as.numeric(fit$slope)
      intercept <- as.numeric(fit$intercept)

      ct <- suppressWarnings(stats::cor.test(t_years, y, method = "spearman", exact = FALSE))
      rho <- unname(ct$estimate)
      pval <- ct$p.value

      # classificacao
      tr <- if (is.finite(pval) && pval < alpha) {
        if (is.finite(slope_py) && slope_py > 0) "aumento" else if (is.finite(slope_py) && slope_py < 0) "queda" else "estavel"
      } else "estavel"

      span_years <- max(t_years) - min(t_years)
      y0 <- intercept
      y1 <- intercept + slope_py * span_years
      pct <- if (is.finite(y0) && y0 != 0) (y1 - y0) / abs(y0) * 100 else NA_real_

      res <- data.frame(
        n = n,
        date_min = min(x_date),
        date_max = max(x_date),
        days_span = as.numeric(diff(range(x_date))),
        slope_per_year = slope_py,
        intercept = intercept,
        rho_spearman = rho,
        p_value = pval,
        trend = tr,
        pct_change_period = pct,
        check.names = FALSE
      )
    }

    meta <- g[1, group_cols, drop = FALSE]
    res <- cbind(meta, param = param, res, row.names = NULL)
    j <- j + 1L
    out[[j]] <- res
  }

  ans <- do.call(rbind, out)
  rownames(ans) <- NULL
  ans
}

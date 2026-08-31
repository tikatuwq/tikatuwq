# R/mk_seasonal.R
# Teste de Mann-Kendall sazonal — Hirsch, Slack & Smith (1982)
# ASCII-only no codigo

# ---- Helpers internos -------------------------------------------------------

# Estatistica S de Mann-Kendall e sua variancia para um vetor
.mk_one <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3L) return(list(S = 0, varS = 0, n = n, valid = FALSE))
  diffs <- outer(x, x, FUN = "-")
  S     <- sum(sign(diffs[lower.tri(diffs)]))
  freq  <- tabulate(match(x, unique(x)))
  tsum  <- sum(vapply(freq[freq > 1L],
                      function(t) t * (t - 1L) * (2L * t + 5L),
                      numeric(1)))
  varS  <- max(0, (n * (n - 1L) * (2L * n + 5L) - tsum) / 18)
  list(S = S, varS = varS, n = n, valid = TRUE)
}

# Inclinacao de Sen: mediana das inclinacoes pareadas (xi - xj)/(ti - tj)
.sens_slope_num <- function(x, t_num) {
  ok    <- is.finite(x) & is.finite(t_num)
  x     <- x[ok]; t_num <- t_num[ok]
  n     <- length(x)
  if (n < 2L) return(NA_real_)
  dt    <- outer(t_num, t_num, "-")
  dx    <- outer(x, x, "-")
  lo    <- lower.tri(dt)
  dts   <- dt[lo]; dxs <- dx[lo]
  valid <- dts != 0 & is.finite(dts) & is.finite(dxs)
  if (!any(valid)) return(NA_real_)
  stats::median(dxs[valid] / dts[valid])
}

# ---- Funcao principal -------------------------------------------------------

#' Teste de Mann-Kendall sazonal
#'
#' @description
#' Implementa o teste de Mann-Kendall sazonal de Hirsch, Slack & Smith (1982),
#' adequado para series temporais de qualidade da agua com sazonalidade
#' (periodos chuvoso/seco ou meses do ano). A estatistica S total e a soma
#' das estatisticas S computadas separadamente por estacao, o que remove
#' o vies introduzido pela sazonalidade nos testes convencionais.
#'
#' @details
#' Algoritmo (Hirsch et al., 1982):
#' \enumerate{
#'   \item Para cada estacao \emph{m}, extrai as observacoes dentro daquela
#'     estacao e calcula \eqn{S_m} e \eqn{VAR(S_m)}.
#'   \item Soma: \eqn{S = \sum S_m}, \eqn{VAR(S) = \sum VAR(S_m)}.
#'   \item Estatistica Z com correcao de continuidade:
#'     \eqn{Z = (S - \text{sgn}(S)) / \sqrt{VAR(S)}}.
#'   \item p-valor bilateral pela distribuicao normal padrao.
#' }
#'
#' A inclinacao de Sen e calculada sobre a serie completa (mediana de todas
#' as inclinacoes pareadas \eqn{(x_j - x_i)/(t_j - t_i)}), expressa em
#' unidades do parametro por ano.
#'
#' Requer ao menos 3 observacoes por estacao para incluir a estacao no calculo;
#' estacoes com menos dados sao ignoradas (com aviso).
#'
#' @param df Data frame com ao menos a coluna do \code{param} e \code{date_col}.
#' @param param Character; nome da coluna do parametro a testar.
#' @param date_col Character; nome da coluna de datas. Default \code{"data"}.
#' @param by Character vector; colunas de agrupamento.
#'   Default \code{"ponto"}.
#' @param period \code{"monthly"} (default) — 12 estacoes (meses 1–12); ou
#'   \code{"season"} — 2 estacoes definidas por \code{season_col}
#'   (requer \code{assign_season()} previamente).
#' @param season_col Character; nome da coluna de periodo hidrologico,
#'   usado apenas quando \code{period = "season"}.
#'   Default \code{"season"}.
#' @param alpha Numeric; nivel de significancia. Default \code{0.05}.
#' @param locale Character; idioma dos rotulos de tendencia:
#'   \code{"pt"} (default) ou \code{"en"}.
#'
#' @returns
#' Um tibble com uma linha por grupo, contendo:
#' \describe{
#'   \item{parametro}{Nome do parametro testado.}
#'   \item{n_obs}{Total de observacoes validas.}
#'   \item{n_estacoes}{Numero de estacoes com dados suficientes (>= 3).}
#'   \item{S}{Estatistica S de Mann-Kendall agregada.}
#'   \item{varS}{Variancia de S.}
#'   \item{Z}{Estatistica Z normalizada.}
#'   \item{p_value}{p-valor bilateral.}
#'   \item{tau}{Tau de Kendall normalizado.}
#'   \item{sen_slope}{Inclinacao de Sen (unidade/ano).}
#'   \item{significativo}{Logical; \code{p_value < alpha}.}
#'   \item{tendencia}{\code{"crescente"}, \code{"decrescente"} ou
#'     \code{"sem_tendencia"}.}
#' }
#'
#' @references
#' Hirsch, R. M., Slack, J. R., & Smith, R. A. (1982). Techniques of trend
#' analysis for monthly water quality data. *Water Resources Research*,
#' 18(1), 107–121. \doi{10.1029/WR018i001p00107}
#'
#' @seealso \code{\link[=assign_season]{assign_season()}},
#'   \code{\link[=trend_param]{trend_param()}}
#'
#' @family trend-tools
#'
#' @importFrom stats median pnorm
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#'
#' @examples
#' data("wq_demo", package = "tikatuwq")
#'
#' # Mann-Kendall mensal (period = "monthly")
#' mk_seasonal(wq_demo, param = "turbidez", by = "ponto")
#'
#' \donttest{
#' # Mann-Kendall por periodo hidrologico
#' d <- assign_season(wq_demo, region = "bahia")
#' mk_seasonal(d, param = "turbidez", by = "ponto",
#'             period = "season", season_col = "season")
#' }
#'
#' @export
mk_seasonal <- function(
  df,
  param,
  date_col   = "data",
  by         = "ponto",
  period     = c("monthly", "season"),
  season_col = "season",
  alpha      = 0.05,
  locale     = c("pt", "en")
) {
  period <- match.arg(period)
  locale <- match.arg(locale)
  stopifnot(is.data.frame(df))

  if (!param %in% names(df))
    stop("Parametro '", param, "' nao encontrado.")
  if (!date_col %in% names(df))
    stop("Coluna '", date_col, "' nao encontrada.")

  if (period == "season" && !season_col %in% names(df))
    stop("Coluna '", season_col, "' nao encontrada. ",
         "Use assign_season() antes ou escolha period = 'monthly'.")

  # Grupos de agrupamento
  by_ok <- intersect(by, names(df))
  if (!length(by_ok)) {
    df[[".__grp__"]] <- "all"
    by_ok <- ".__grp__"
  }

  dates  <- as.Date(df[[date_col]])
  vals   <- suppressWarnings(as.numeric(df[[param]]))
  t_num  <- as.numeric(dates) / 365.25  # escala em anos

  # Definir estacoes
  if (period == "monthly") {
    seasons_vec <- as.integer(format(dates, "%m"))
  } else {
    seasons_vec <- df[[season_col]]
  }

  grps <- split(seq_len(nrow(df)), df[by_ok], drop = TRUE)

  result_list <- lapply(grps, function(idx) {
    v      <- vals[idx]
    t_v    <- t_num[idx]
    s_v    <- seasons_vec[idx]
    row_ids <- as.list(df[idx[1], by_ok, drop = FALSE])

    season_levels <- sort(unique(s_v[!is.na(s_v)]))
    n_valid_total <- sum(is.finite(v))

    S_total    <- 0
    varS_total <- 0
    n_used     <- 0L

    for (lev in season_levels) {
      mask  <- !is.na(s_v) & s_v == lev
      v_m   <- v[mask]
      t_m   <- t_v[mask]
      ord   <- order(t_m)   # garantir ordem cronologica
      v_m   <- v_m[ord]
      res   <- .mk_one(v_m)
      if (res$valid) {
        S_total    <- S_total + res$S
        varS_total <- varS_total + res$varS
        n_used     <- n_used + 1L
      }
    }

    if (n_used < 2L) {
      warning("Grupo '", paste(unlist(row_ids), collapse = "/"), "': ",
              "menos de 2 estacoes com dados suficientes para o teste.")
    }

    # Z com correcao de continuidade
    Z_stat <- NA_real_
    p_val  <- NA_real_
    if (varS_total > 0 && n_used >= 2L) {
      Z_stat <- (S_total - sign(S_total)) / sqrt(varS_total)
      p_val  <- 2 * (1 - stats::pnorm(abs(Z_stat)))
    }

    # Tau de Kendall
    tau <- NA_real_
    if (n_valid_total > 1L) {
      n_pairs <- n_valid_total * (n_valid_total - 1L) / 2
      if (n_pairs > 0) tau <- round(S_total / n_pairs, 4)
    }

    # Inclinacao de Sen (unidade/ano)
    slope <- .sens_slope_num(v, t_v)

    sig <- isTRUE(!is.na(p_val) && p_val < alpha)

    if (locale == "pt") {
      tend <- if (!sig) "sem_tendencia" else if (!is.na(Z_stat) && Z_stat > 0) "crescente" else "decrescente"
    } else {
      tend <- if (!sig) "no_trend" else if (!is.na(Z_stat) && Z_stat > 0) "increasing" else "decreasing"
    }

    base <- as.data.frame(row_ids, stringsAsFactors = FALSE)
    base[["parametro"]]     <- param
    base[["n_obs"]]         <- n_valid_total
    base[["n_estacoes"]]    <- n_used
    base[["S"]]             <- S_total
    base[["varS"]]          <- round(varS_total, 2)
    base[["Z"]]             <- round(Z_stat, 4)
    base[["p_value"]]       <- round(p_val,  4)
    base[["tau"]]           <- tau
    base[["sen_slope"]]     <- round(slope,  6)
    base[["significativo"]] <- sig
    base[["tendencia"]]     <- tend
    base
  })

  out <- tibble::as_tibble(dplyr::bind_rows(result_list))
  out[[".__grp__"]] <- NULL
  out
}

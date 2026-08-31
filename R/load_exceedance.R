# R/load_exceedance.R
# Carga poluidora e probabilidade de excedencia de limites
# (ASCII-only no codigo)

#' Carga poluidora (concentracao x vazao)
#'
#' @description
#' Calcula a carga poluidora diaria (ou na unidade desejada) como o
#' produto entre concentracao e vazao: \eqn{L = C \times Q \times f}{L = C * Q * f},
#' onde \eqn{f} e o fator de conversao de unidades.
#'
#' @details
#' Unidades de saida suportadas:
#' \describe{
#'   \item{\code{"kg_dia"}}{mg/L * m3/s -> kg/dia (fator = 86.4).}
#'   \item{\code{"t_dia"}}{mg/L * m3/s -> t/dia (fator = 0.0864).}
#'   \item{\code{"kg_ano"}}{mg/L * m3/s -> kg/ano (fator = 31536).}
#'   \item{\code{"g_s"}}{mg/L * m3/s -> g/s (fator = 1).}
#' }
#' As unidades pressupõem vazao em m³/s e concentracao em mg/L.
#' Ajuste o fator com \code{unit_factor} se necessario.
#'
#' @param df Data frame com ao menos a coluna de \code{param} e de \code{flow_col}.
#' @param param Character; nome da coluna de concentracao (mg/L).
#' @param flow_col Character; nome da coluna de vazao. Default \code{"vazao"}.
#' @param unit_out Character; unidade de saida. Uma de
#'   \code{"kg_dia"} (default), \code{"t_dia"}, \code{"kg_ano"}, \code{"g_s"}.
#' @param unit_factor Numeric; fator de conversao personalizado. Se fornecido,
#'   sobrepoe \code{unit_out}.
#' @param col_name Character; nome da coluna de saida. Default: composto
#'   automaticamente como \code{"{param}_carga_{unit_out}"}.
#'
#' @returns O \code{df} de entrada com a nova coluna de carga adicionada.
#'
#' @seealso \code{\link[=exceedance_prob]{exceedance_prob()}}
#'
#' @family load-tools
#'
#' @examples
#' \donttest{
#' data("wq_demo", package = "tikatuwq")
#' # Adiciona vazao ficticia para demonstracao
#' d <- wq_demo
#' d$vazao <- runif(nrow(d), 2, 10)
#' d <- compute_load(d, param = "p_total", flow_col = "vazao", unit_out = "kg_dia")
#' head(d[, c("ponto","p_total","vazao","p_total_carga_kg_dia")])
#' }
#'
#' @export
compute_load <- function(
  df,
  param,
  flow_col    = "vazao",
  unit_out    = c("kg_dia", "t_dia", "kg_ano", "g_s"),
  unit_factor = NULL,
  col_name    = NULL
) {
  unit_out <- match.arg(unit_out)
  stopifnot(is.data.frame(df))
  if (!param    %in% names(df)) stop("Coluna '", param,    "' nao encontrada.")
  if (!flow_col %in% names(df)) stop("Coluna '", flow_col, "' nao encontrada.")

  # Fatores de conversao (mg/L * m3/s -> unidade alvo)
  factors <- c(kg_dia = 86.4, t_dia = 0.0864, kg_ano = 31536, g_s = 1)

  f <- if (!is.null(unit_factor)) as.numeric(unit_factor) else factors[[unit_out]]

  conc  <- suppressWarnings(as.numeric(df[[param]]))
  flow  <- suppressWarnings(as.numeric(df[[flow_col]]))
  carga <- conc * flow * f

  if (is.null(col_name)) col_name <- paste0(param, "_carga_", unit_out)
  df[[col_name]] <- carga
  df
}

#' Probabilidade de excedencia de um limite
#'
#' @description
#' Calcula a probabilidade empirica de que um parametro de qualidade da
#' agua exceda um determinado valor de referencia (threshold), por grupo.
#' Util para analise de risco ambiental e pre-avaliacao de conformidade.
#'
#' @details
#' A probabilidade e calculada como:
#' \eqn{P_{ex} = n_{ex} / n_{valid}}{P_ex = n_ex / n_valid}
#' onde \eqn{n_{ex}} e o numero de amostras que excedem \code{threshold}
#' e \eqn{n_{valid}} e o total de amostras com valor valido (nao-NA).
#'
#' Para parametros com limite minimo (ex.: OD), usar \code{direction = "below"}.
#' Para intervalos (ex.: pH 6-9), chamar duas vezes ou usar
#' \code{conama_freq_check()}.
#'
#' @param df Data frame com ao menos a coluna do \code{param} e as colunas
#'   indicadas em \code{by}.
#' @param param Character; nome do parametro a avaliar.
#' @param threshold Numeric; valor de referencia.
#' @param direction \code{"above"} (default) — excedencia acima do limite
#'   (ex.: turbidez, DBO, coliformes); ou \code{"below"} — excedencia
#'   abaixo do limite (ex.: OD, saturacao de oxigenio).
#' @param by Character vector; colunas de agrupamento.
#'   Default \code{"ponto"}.
#' @param conf_level Numeric; nivel de confianca para o intervalo de
#'   confianca de Wilson. Default \code{0.95}.
#'
#' @returns
#' Um tibble com uma linha por grupo contendo:
#' \describe{
#'   \item{n}{Total de amostras validas no grupo.}
#'   \item{n_excedeu}{Amostras que excedem o threshold.}
#'   \item{prob_excedencia}{Probabilidade empirica (0-1).}
#'   \item{ic_inf, ic_sup}{Intervalo de confianca de Wilson para a proporcao.}
#' }
#'
#' @seealso \code{\link[=conama_freq_check]{conama_freq_check()}}
#'
#' @family load-tools
#'
#' @importFrom stats qnorm
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#'
#' @examples
#' data("wq_demo", package = "tikatuwq")
#' # Probabilidade de turbidez acima de 40 NTU (limite CONAMA classe 1)
#' exceedance_prob(wq_demo, param = "turbidez", threshold = 40, by = "ponto")
#'
#' # Probabilidade de OD abaixo de 5 mg/L (limite CONAMA classe 2)
#' exceedance_prob(wq_demo, param = "od", threshold = 5,
#'                 direction = "below", by = "ponto")
#'
#' @export
exceedance_prob <- function(
  df,
  param,
  threshold,
  direction  = c("above", "below"),
  by         = "ponto",
  conf_level = 0.95
) {
  direction <- match.arg(direction)
  stopifnot(is.data.frame(df), is.numeric(threshold), length(threshold) == 1)
  if (!param %in% names(df)) stop("Coluna '", param, "' nao encontrada.")

  by_ok <- intersect(by, names(df))
  if (!length(by_ok)) {
    df[[".__grp__"]] <- "all"
    by_ok <- ".__grp__"
  }

  vals <- suppressWarnings(as.numeric(df[[param]]))
  z    <- stats::qnorm(1 - (1 - conf_level) / 2)

  grps        <- split(seq_len(nrow(df)), df[by_ok], drop = TRUE)
  result_list <- lapply(grps, function(idx) {
    v      <- vals[idx]
    v_ok   <- v[is.finite(v)]
    n_val  <- length(v_ok)
    n_exc  <- if (direction == "above") sum(v_ok > threshold) else sum(v_ok < threshold)
    p_hat  <- if (n_val > 0) n_exc / n_val else NA_real_

    # Intervalo de confianca de Wilson
    ic_inf <- NA_real_
    ic_sup <- NA_real_
    if (!is.na(p_hat) && n_val > 0) {
      denom  <- 1 + z^2 / n_val
      centre <- (p_hat + z^2 / (2 * n_val)) / denom
      half   <- z * sqrt(p_hat * (1 - p_hat) / n_val + z^2 / (4 * n_val^2)) / denom
      ic_inf <- max(0, centre - half)
      ic_sup <- min(1, centre + half)
    }

    row_ids <- as.list(df[idx[1], by_ok, drop = FALSE])
    base    <- as.data.frame(row_ids, stringsAsFactors = FALSE)
    base[["threshold"]]       <- threshold
    base[["direction"]]       <- direction
    base[["n"]]               <- n_val
    base[["n_excedeu"]]       <- n_exc
    base[["prob_excedencia"]] <- round(p_hat, 4)
    base[["ic_inf"]]          <- round(ic_inf, 4)
    base[["ic_sup"]]          <- round(ic_sup, 4)
    base
  })

  out <- tibble::as_tibble(dplyr::bind_rows(result_list))
  out[[".__grp__"]] <- NULL
  out
}

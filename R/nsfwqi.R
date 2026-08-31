# R/nsfwqi.R
# National Sanitation Foundation Water Quality Index (NSF WQI)
# (ASCII-only no codigo)

#' National Sanitation Foundation Water Quality Index (NSF WQI)
#'
#' @description
#' Calcula o NSF WQI suportando a agregacao multiplicativa (McClelland / NSF revisado:
#' \eqn{WQI = \prod Q_i^{w_i}}{WQI = prod(Qi^Wi)}) e a agregacao aditiva original (Brown et al., 1970:
#' \eqn{WQI = \sum Q_i \cdot w_i}{WQI = sum(Qi * Wi)}).
#'
#' @details
#' O mapeamento de parametros tenta resolver colunas em formato brasileiro ou internacional:
#' \describe{
#'   \item{\code{do}}{Oxigenio dissolvido (mg/L ou sat).}

#'   \item{\code{fc}}{Coliformes termotolerantes / fecais (NMP/100 mL).}
#'   \item{\code{ph}}{pH (unidades).}
#'   \item{\code{bod}}{Demanda bioquimica de oxigenio (mg/L).}
#'   \item{\code{temp_change}}{Variacao termica Delta T (graus Celsius).}
#'   \item{\code{po4}}{Fosfato total (mg/L).}
#'   \item{\code{no3}}{Nitrato (mg/L).}
#'   \item{\code{turbidez}}{Turbidez (NTU).}
#'   \item{\code{ts}}{Solidos totais (mg/L).}
#' }
#'
#' @param df Data frame com as colunas dos parametros.
#' @param pesos Vetor nomeado numerico com os pesos oficiais NSF:
#'   \code{do=0.17}, \code{fc=0.16}, \code{ph=0.11}, \code{bod=0.11},
#'   \code{temp_change=0.10}, \code{po4=0.10}, \code{no3=0.10},
#'   \code{turbidez=0.08}, \code{ts=0.07}.
#' @param aggregation Metodo de agregacao: \code{"mcclelland_geometric"} (padrao geometrico ponderado)
#'   ou \code{"brown1970_arithmetic"} (padrao aritmetico ponderado original de 1970).
#' @param na_rm Logico; se \code{TRUE}, renormaliza os pesos por linha para os parametros validos.
#'   Se \code{FALSE} (padrao), linhas com parametros faltantes resultam em \code{NA}.
#' @param add_status Logico; se \code{TRUE} (padrao), adiciona a coluna \code{NSFWQI_status}.
#' @param locale Idioma dos rotulos: \code{"pt"} (padrao) ou \code{"en"}.
#'
#' @returns O data frame de entrada com a coluna \code{NSFWQI} e, opcionalmente, \code{NSFWQI_status}.
#'
#' @references
#' Brown, R. M., McClelland, N. I., Deininger, R. A., & Tozer, R. G. (1970).
#' A water quality index - do we dare? \emph{Water and Sewage Works}, 117(10), 339-343.
#'
#' McClelland, N. I. (1974). \emph{Water Quality Index Application in the Kansas River Basin}.
#' EPA-907/9-74-001, U.S. Environmental Protection Agency.
#'
#' @family water-quality-indices
#' @export
#' @examples
#' d <- data.frame(
#'   od = 6.5, coliformes = 200, ph = 7.2, dbo = 2,
#'   temperatura = 25, p_ortofosfato = 0.05, n_nitrato = 1.0,
#'   turbidez = 8, solidos_totais = 120
#' )
#' nsfwqi(d, na_rm = TRUE)
nsfwqi <- function(
  df,
  pesos = c(
    do = 0.17, fc = 0.16, ph = 0.11, bod = 0.11, temp_change = 0.10,
    po4 = 0.10, no3 = 0.10, turbidez = 0.08, ts = 0.07
  ),
  aggregation = c("mcclelland_geometric", "brown1970_arithmetic"),
  na_rm      = FALSE,
  add_status = TRUE,
  locale     = c("pt", "en")
) {
  aggregation <- match.arg(aggregation)
  locale      <- match.arg(locale)
  stopifnot(is.data.frame(df))

  if (nrow(df) == 0L) {
    out <- df
    out$NSFWQI <- numeric(0)
    if (isTRUE(add_status)) out$NSFWQI_status <- factor()
    return(out)
  }

  .rc <- function(canonical) .resolve_col(df, canonical)

  work <- list()
  if (!is.null(.rc("od")))                 work$do          <- .parse_nd_ld(df[[.rc("od")]], "limit")
  if (!is.null(.rc("coliformes_termotolerantes"))) work$fc   <- .parse_nd_ld(df[[.rc("coliformes_termotolerantes")]], "limit")
  if (!is.null(.rc("ph")))                 work$ph          <- .parse_nd_ld(df[[.rc("ph")]], "limit")
  if (!is.null(.rc("dbo")))                work$bod         <- .parse_nd_ld(df[[.rc("dbo")]], "limit")
  if (!is.null(.rc("turbidez")))           work$turbidez    <- .parse_nd_ld(df[[.rc("turbidez")]], "limit")
  if (!is.null(.rc("solidos_totais")))     work$ts          <- .parse_nd_ld(df[[.rc("solidos_totais")]], "limit")
  if (!is.null(.rc("solidos_suspensos")) && is.null(work$ts)) work$ts <- .parse_nd_ld(df[[.rc("solidos_suspensos")]], "limit")
  if (!is.null(.rc("p_ortofosfato")))      work$po4         <- .parse_nd_ld(df[[.rc("p_ortofosfato")]], "limit")
  if (!is.null(.rc("p_total")) && is.null(work$po4)) work$po4 <- .parse_nd_ld(df[[.rc("p_total")]], "limit") * 3.066
  if (!is.null(.rc("n_nitrato")))          work$no3         <- .parse_nd_ld(df[[.rc("n_nitrato")]], "limit")
  if (!is.null(.rc("delta_temperatura")))  work$temp_change <- .parse_nd_ld(df[[.rc("delta_temperatura")]], "limit")
  else if ("temp_change" %in% names(df))   work$temp_change <- .parse_nd_ld(df[["temp_change"]], "limit")
  else work$temp_change <- rep(0, nrow(df)) # default Delta T = 0

  wdf  <- as.data.frame(work, stringsAsFactors = FALSE)
  use  <- intersect(names(pesos), names(wdf))
  if (!length(use)) stop("Nenhuma coluna compativel com NSFWQI encontrada.")

  wdf    <- wdf[use]
  w_full <- pesos[use]

  # Funcoes de sub-indice Qi aproximadas baseadas nas curvas NSF
  qi_piecewise <- function(param, v) {
    if (!is.finite(v)) return(NA_real_)
    switch(
      param,
      "do"          = ifelse(v >= 7.5, 90, ifelse(v >= 6, 80, ifelse(v >= 5, 70, 50))),
      "fc"          = ifelse(v <= 200, 90, ifelse(v <= 1000, 70, 40)),
      "ph"          = ifelse(v >= 6.5 & v <= 8.5, 90, 60),
      "bod"         = ifelse(v <= 3, 90, ifelse(v <= 5, 75, 55)),
      "temp_change" = ifelse(abs(v) <= 2, 90, ifelse(abs(v) <= 5, 75, 55)),
      "po4"         = ifelse(v <= 0.05, 90, ifelse(v <= 0.10, 75, 55)),
      "no3"         = ifelse(v <= 1, 90, ifelse(v <= 10, 70, 50)),
      "turbidez"    = ifelse(v <= 5, 90, ifelse(v <= 50, 70, 50)),
      "ts"          = ifelse(v <= 150, 85, ifelse(v <= 500, 65, 35)),
      50
    )
  }

  qi_mat <- matrix(NA_real_, nrow = nrow(wdf), ncol = length(use), dimnames = list(NULL, use))
  for (j in seq_along(use)) {
    p <- use[j]
    qi_mat[, j] <- vapply(
      suppressWarnings(as.numeric(wdf[[p]])),
      function(x) qi_piecewise(p, x),
      numeric(1)
    )
  }

  NSFWQI_val <- vapply(seq_len(nrow(wdf)), function(i) {
    qi_row <- qi_mat[i, ]
    ok     <- is.finite(qi_row) & qi_row > 0
    if (!any(ok)) return(NA_real_)
    if (!na_rm && !all(ok)) return(NA_real_)

    ww <- w_full[ok] / sum(w_full[ok])
    if (aggregation == "brown1970_arithmetic") {
      sum(qi_row[ok] * ww)
    } else {
      prod(qi_row[ok] ^ ww)
    }
  }, numeric(1))

  out         <- df
  out$NSFWQI  <- round(pmin(100, pmax(0, NSFWQI_val)), 1)

  if (add_status) {
    if (locale == "pt") {
      status_labels <- c("Muito Ruim", "Ruim", "Regular", "Boa", "Excelente")
    } else {
      status_labels <- c("Very Bad", "Bad", "Fair", "Good", "Excellent")
    }
    breaks_st <- c(-Inf, 25, 50, 70, 90, Inf)
    out$NSFWQI_status <- as.character(
      cut(out$NSFWQI, breaks = breaks_st, labels = status_labels, right = TRUE)
    )
    out$NSFWQI_status[is.na(out$NSFWQI)] <- NA_character_
  }

  attr(out, "nsfwqi_aggregation") <- aggregation
  out
}

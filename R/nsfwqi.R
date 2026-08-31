# R/nsfwqi.R
# NSF Water Quality Index (Brown et al. 1970) — ASCII-only no codigo

#' NSF Water Quality Index (NSF WQI)
#'
#' @description
#' Calcula o NSF WQI (Brown et al., 1970) como media geometrica ponderada
#' dos sub-escores dos parametros: \eqn{WQI = \prod Q_i^{w_i}}{WQI = prod(Qi^Wi)}.
#' Aceita nomes de colunas no padrao brasileiro (e.g. \code{od}, \code{dbo},
#' \code{coliformes}) e traduz automaticamente para os indices NSF.
#'
#' @details
#' O mapeamento tentado (alias BR -> nome NSF) e:
#' \describe{
#'   \item{\code{do}}{coluna \code{od} ou \code{do}.}
#'   \item{\code{fc}}{coluna \code{coliformes} ou \code{fc}.}
#'   \item{\code{ph}}{coluna \code{pH}, \code{ph} ou \code{pH}.}
#'   \item{\code{bod}}{coluna \code{dbo} ou \code{bod}.}
#'   \item{\code{turbidez}}{coluna \code{turbidez}.}
#'   \item{\code{sst}}{coluna \code{solidos_suspensos} ou \code{sst}.}
#'   \item{\code{po4}}{coluna \code{p_ortofosfato} ou \code{po4}.}
#'   \item{\code{no3}}{coluna \code{n_nitrato} ou \code{no3}.}
#'   \item{\code{temp_change}}{coluna \code{temp_change} (delta T relativo
#'     ao padrao; deve ser calculado externamente).}
#' }
#'
#' Os Qi (sub-escores, escala 0-100) seguem curvas piecewise baseadas nas
#' curvas originais de Brown et al. (1970). A agregacao usa media geometrica
#' ponderada, fiel ao metodo original.
#'
#' Se \code{na_rm = TRUE}, os pesos sao renormalizados por linha aos
#' parametros disponiveis. Se \code{na_rm = FALSE} (default), linhas com
#' qualquer NA resultam em \code{NSFWQI = NA} (sem erro).
#'
#' @param df Data frame com as colunas de parametros (ver Detalhes).
#' @param pesos Named numeric vector com os pesos dos parametros.
#'   Os defaults seguem a proposta original NSF:
#'   \code{do=.17}, \code{fc=.16}, \code{ph=.11}, \code{bod=.11},
#'   \code{temp_change=.10}, \code{po4=.10}, \code{no3=.10},
#'   \code{turbidez=.08}, \code{sst=.07}.
#' @param na_rm Logical; se \code{TRUE}, renormaliza pesos por linha para
#'   os parametros com valor valido. Default \code{FALSE}.
#' @param add_status Logical; adiciona a coluna \code{NSFWQI_status} com a
#'   classificacao qualitativa. Default \code{TRUE}.
#' @param locale Character; idioma para os rotulos de status:
#'   \code{"pt"} (default, portugues) ou \code{"en"} (ingles).
#'
#' @returns O data frame de entrada com a coluna \code{NSFWQI} (e
#'   opcionalmente \code{NSFWQI_status}) adicionada.
#'
#' @references Brown, R. M., McClelland, N. I., Deininger, R. A., &
#'   Tozer, R. G. (1970). A water quality index - do we dare? *Water and
#'   Sewage Works*, 117(10), 339-343.
#'
#' @seealso \code{\link[=iqa]{iqa()}}, \code{\link[=plot_iqa]{plot_iqa()}}
#'
#' @family wqi-tools
#'
#' @importFrom tibble as_tibble
#'
#' @examples
#' \donttest{
#' data("wq_demo", package = "tikatuwq")
#' d <- wq_demo
#' # Mapeia alias brasileiros
#' d$do  <- d$od
#' d$fc  <- d$coliformes
#' d$bod <- d$dbo
#' # Parametros ausentes sao ignorados com na_rm = TRUE
#' out <- nsfwqi(d, na_rm = TRUE)
#' head(out[, c("ponto", "NSFWQI", "NSFWQI_status")])
#' }
#'
#' @export
nsfwqi <- function(
  df,
  pesos = c(
    do = 0.17, fc = 0.16, ph = 0.11, bod = 0.11, temp_change = 0.10,
    po4 = 0.10, no3 = 0.10, turbidez = 0.08, sst = 0.07
  ),
  na_rm      = FALSE,
  add_status = TRUE,
  locale     = c("pt", "en")
) {
  locale <- match.arg(locale)
  stopifnot(is.data.frame(df))

  # ---- Mapeamento de colunas via dicionario central (.param_aliases) ---------
  .rc <- function(canonical) .resolve_col(df, canonical)

  work <- list()
  if (!is.null(.rc("od")))               work$do         <- df[[.rc("od")]]
  if (!is.null(.rc("coliformes")))        work$fc         <- df[[.rc("coliformes")]]
  if (!is.null(.rc("ph")))               work$ph         <- df[[.rc("ph")]]
  if (!is.null(.rc("dbo")))              work$bod        <- df[[.rc("dbo")]]
  if (!is.null(.rc("turbidez")))         work$turbidez   <- df[[.rc("turbidez")]]
  if (!is.null(.rc("solidos_suspensos"))) work$sst       <- df[[.rc("solidos_suspensos")]]
  if (!is.null(.rc("p_ortofosfato")))    work$po4        <- df[[.rc("p_ortofosfato")]]
  if (!is.null(.rc("n_nitrato")))        work$no3        <- df[[.rc("n_nitrato")]]
  if ("temp_change" %in% names(df))      work$temp_change <- df$temp_change

  if (!length(work)) stop("Nenhuma coluna compativel com NSFWQI encontrada.")

  wdf  <- as.data.frame(work, stringsAsFactors = FALSE)
  use  <- intersect(names(pesos), names(wdf))
  if (!length(use)) stop("Nenhuma coluna disponivel entre: ", paste(names(pesos), collapse = ", "))

  wdf    <- wdf[use]
  w_full <- pesos[use]

  # ---- Funcoes Qi (piecewise baseado em Brown et al., 1970) -----------------
  qi_piecewise <- function(param, v) {
    if (!is.finite(v)) return(NA_real_)
    switch(
      param,
      "do"          = ifelse(v >= 7.5, 90, ifelse(v >= 6, 80, ifelse(v >= 5, 70, 50))),
      "fc"          = ifelse(v <= 200, 90, ifelse(v <= 1000, 70, 40)),
      "ph"          = ifelse(v >= 6.5 & v <= 8.5, 90, 60),
      "bod"         = ifelse(v <= 3, 90, ifelse(v <= 5, 75, 55)),
      "temp_change" = ifelse(v <= 2, 90, ifelse(v <= 5, 75, 55)),
      "po4"         = ifelse(v <= 0.05, 90, ifelse(v <= 0.10, 75, 55)),
      "no3"         = ifelse(v <= 1, 90, ifelse(v <= 10, 70, 50)),
      "turbidez"    = ifelse(v <= 5, 90, ifelse(v <= 50, 70, 50)),
      "sst"         = ifelse(v <= 500, 85, 60),
      50
    )
  }

  # Matriz Qi
  qi_mat <- matrix(NA_real_, nrow = nrow(wdf), ncol = length(use),
                   dimnames = list(NULL, use))
  for (j in seq_along(use)) {
    p <- use[j]
    qi_mat[, j] <- vapply(
      suppressWarnings(as.numeric(wdf[[p]])),
      function(x) qi_piecewise(p, x),
      numeric(1)
    )
  }

  # ---- Agregacao: media geometrica ponderada --------------------------------
  NSFWQI_val <- vapply(seq_len(nrow(wdf)), function(i) {
    qi_row <- qi_mat[i, ]
    ok     <- is.finite(qi_row) & qi_row > 0
    if (!any(ok)) return(NA_real_)
    if (na_rm) {
      ww <- w_full[ok]
      ww <- ww / sum(ww)
    } else {
      if (!all(ok)) return(NA_real_)
      ww <- w_full / sum(w_full)
    }
    prod(qi_row[ok]^ww)
  }, numeric(1))

  out         <- df
  out$NSFWQI  <- round(NSFWQI_val, 1)

  # ---- Status ---------------------------------------------------------------
  if (add_status) {
    if (locale == "pt") {
      status_labels <- c(
        "Muito Ruim",  # 0 - 25
        "Ruim",        # 25 - 50
        "Regular",     # 50 - 70
        "Boa",         # 70 - 90
        "Excelente"    # 90 - 100
      )
    } else {
      status_labels <- c("Very Bad", "Bad", "Fair", "Good", "Excellent")
    }
    breaks_st <- c(-Inf, 25, 50, 70, 90, Inf)
    out$NSFWQI_status <- as.character(
      cut(out$NSFWQI, breaks = breaks_st, labels = status_labels, right = TRUE)
    )
    out$NSFWQI_status[is.na(out$NSFWQI)] <- NA_character_
  }

  out
}

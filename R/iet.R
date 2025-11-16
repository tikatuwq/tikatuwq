# R/iet.R
# Trophic State Index (Carlson) - implementacao base
# (ASCII-only no codigo)

#' Classifica TSI (Carlson) em faixas qualitativas
#'
#' @description
#' Converte valores do indice trofico de Carlson (TSI/IET) para classes
#' qualitativas ordenadas. Retorna fator ordenado em portugues ("pt")
#' ou ingles ("en").
#'
#' @param x Vetor numerico com TSI/IET (0-100). NA preservado.
#' @param locale Idioma dos rotulos: \code{"pt"} (padrao) ou \code{"en"}.
#'
#' @return Fator ordenado com classes de trofia.
#'
#' @examples
#' classify_tsi_carlson(c(25, 35, 45, 60, 80))
#' classify_tsi_carlson(c(25, 35, 45, 60, 80), locale = "en")
#'
#' @export
classify_tsi_carlson <- function(x, locale = c("pt", "en")) {
  locale <- match.arg(locale)
  # Carlson (1977): <30 Ultraoligo; 30-40 Oligo; 40-50 Meso; 50-70 Eutro; >70 Hipereutro
  breaks <- c(-Inf, 30, 40, 50, 70, Inf)

  if (locale == "pt") {
    labs <- c("Ultraoligotrofico", "Oligotrofico", "Mesotrofico",
              "Eutrofico", "Hipereutrofico")
  } else {
    labs <- c("Ultra-oligotrophic", "Oligotrophic", "Mesotrophic",
              "Eutrophic", "Hypereutrophic")
  }

  cut(x, breaks = breaks, labels = labs, right = TRUE, ordered_result = TRUE)
}

#' Trophic State Index (Carlson)
#'
#' @description
#' Computa o indice trofico de Carlson (TSI/IET) a partir de profundidade
#' de disco de Secchi, clorofila-a e fosforo total. Retorna componentes
#' e o IET como media por linha dos componentes disponiveis.
#'
#' Pode receber um \code{data.frame} como primeiro argumento (ver Detalhes).
#'
#' @param secchi Vetor numerico com profundidade de Secchi (m) **ou**
#'   um data.frame contendo colunas \code{secchi} (m), \code{clorofila} (ug/L)
#'   e \code{tp} (ug/L) ou \code{p_total} (mg/L). Se for data.frame,
#'   \code{clorofila} e \code{tp} devem ser \code{NULL}.
#' @param clorofila Vetor numerico com clorofila-a (ug/L).
#' @param tp Vetor numerico com fosforo total (ug/L).
#' @param .keep_ids Logico; quando data.frame, vincula colunas de ID
#'   comuns (\code{rio}, \code{ponto}, \code{data}, \code{lat}, \code{lon}).
#'   Padrao \code{FALSE}.
#' @param add_status Logico; se \code{TRUE} (padrao), adiciona a coluna
#'   \code{TSI_status} com a classificacao qualitativa (Carlson).
#' @param locale Idioma de \code{TSI_status}: \code{"pt"} (padrao) ou \code{"en"}.
#' @param ... Reservado para uso futuro (ignorado).
#'
#' @details
#' Formulas implementadas (Carlson 1977):
#' \itemize{
#'   \item \code{TSI_Secchi = 60 - 14.41 * log10(secchi)}
#'   \item \code{TSI_Chla  = 9.81 * log10(clorofila) + 30.6}
#'   \item \code{TSI_TP    = 14.42 * log10(tp) + 4.15}
#' }
#'
#' Quando um data.frame e fornecido, strings com virgula decimal (ex.: "3,2")
#' ou sinais de desigualdade (ex.: "<0,1") sao convertidas com seguranca.
#' Se existir \code{p_total} (mg/L) em vez de \code{tp} (ug/L),
#' e feita conversao interna (\code{tp = p_total * 1000}).
#'
#' Os componentes e o IET final sao limitados ao intervalo \code{[0, 100]}
#' para manter consistencia com as figuras e tabelas do pacote/artigo.
#'
#' @returns
#' Um data.frame com colunas (quando aplicavel):
#' \itemize{
#'   \item \code{TSI_Secchi} — componente de Secchi (0-100).
#'   \item \code{TSI_Chla} — componente de clorofila-a (0-100).
#'   \item \code{TSI_TP} — componente de fosforo total (0-100).
#'   \item \code{IET} — indice Carlson agregado (media por linha, 0-100).
#'   \item \code{TSI_status} — classe qualitativa (quando \code{add_status=TRUE}).
#' }
#'
#' @references
#' Carlson, R. E. (1977). A trophic state index for lakes.
#' Limnology and Oceanography, 22(2), 361-369. doi:10.4319/lo.1977.22.2.0361
#'
#' @seealso \code{\link[=iet_lamparelli]{iet_lamparelli()}},
#'   \code{\link[=iqa]{iqa()}}, \code{\link[=conama_check]{conama_check()}}
#'
#' @examples
#' # Vetores
#' secchi <- c(1.2, 0.8, 0.4)        # m
#' clorofila <- c(5, 12, 30)         # ug/L
#' tp <- c(20, 40, 70)               # ug/L
#' iet_carlson(secchi = secchi, clorofila = clorofila, tp = tp)
#'
#' # Data frame
#' # df <- data.frame(secchi = secchi, clorofila = clorofila, p_total = c(0.02, 0.04, 0.07))
#' # iet_carlson(df)                  # converte p_total -> tp (ug/L)
#' # iet_carlson(df, .keep_ids = TRUE)
#'
#' @export
iet_carlson <- function(secchi = NULL,
                        clorofila = NULL,
                        tp = NULL,
                        .keep_ids = FALSE,
                        add_status = TRUE,
                        locale = c("pt", "en"),
                        ...) {

  locale <- match.arg(locale)

  # Funcoes internas auxiliares
  clip01 <- function(x) pmin(100, pmax(0, x))

  # --- modo data.frame (opcional): secchi eh um DF e demais NULL ---
  if (is.data.frame(secchi) && is.null(clorofila) && is.null(tp)) {
    ext <- .df_extract_iet(secchi)  # supoe helper existente no pacote

    res <- list()
    if (!is.null(ext$secchi))
      res$TSI_Secchi <- 60 - 14.41 * log10(pmax(ext$secchi, 0.001))
    if (!is.null(ext$clorofila))
      res$TSI_Chla  <- 9.81 * log10(pmax(ext$clorofila, 0.001)) + 30.6
    if (!is.null(ext$tp))
      res$TSI_TP    <- 14.42 * log10(pmax(ext$tp, 0.001)) + 4.15

    df_out <- as.data.frame(res, optional = TRUE)

    # clip de componentes
    for (nm in intersect(names(df_out), c("TSI_Secchi","TSI_Chla","TSI_TP"))) {
      df_out[[nm]] <- clip01(df_out[[nm]])
    }

    # agrega IET (media por linha do que existir)
    if (ncol(df_out) > 1) {
      df_out$IET <- rowMeans(df_out, na.rm = TRUE)
    } else if (ncol(df_out) == 1) {
      df_out$IET <- df_out[[1]]
    } else {
      df_out <- data.frame(IET = numeric(0))
    }
    df_out$IET <- clip01(df_out$IET)

    if (isTRUE(add_status) && nrow(df_out)) {
      df_out$TSI_status <- classify_tsi_carlson(df_out$IET, locale = locale)
    }

    if (.keep_ids && nrow(ext$ids)) df_out <- cbind(ext$ids, df_out)
    attr(df_out, "iet_method") <- "Carlson1977"
    return(df_out)
  }

  # --- modo tradicional (vetorial) — preservado ---
  secchi    <- .numify(secchi)
  clorofila <- .numify(clorofila)
  tp        <- .numify(tp)

  res <- list()
  if (!is.null(secchi))
    res$TSI_Secchi <- 60 - 14.41 * log10(pmax(secchi, 0.001))
  if (!is.null(clorofila))
    res$TSI_Chla  <- 9.81 * log10(pmax(clorofila, 0.001)) + 30.6
  if (!is.null(tp))
    res$TSI_TP    <- 14.42 * log10(pmax(tp, 0.001)) + 4.15

  df <- as.data.frame(res, optional = TRUE)

  # clip de componentes
  for (nm in intersect(names(df), c("TSI_Secchi","TSI_Chla","TSI_TP"))) {
    df[[nm]] <- clip01(df[[nm]])
  }

  if (ncol(df) > 1) {
    df$IET <- rowMeans(df, na.rm = TRUE)
  } else if (ncol(df) == 1) {
    df$IET <- df[[1]]
  } else {
    df <- data.frame(IET = numeric(0))
  }
  df$IET <- clip01(df$IET)

  if (isTRUE(add_status) && nrow(df)) {
    df$TSI_status <- classify_tsi_carlson(df$IET, locale = locale)
  }

  attr(df, "iet_method") <- "Carlson1977"
  df
}

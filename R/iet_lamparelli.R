# R/iet_lamparelli.R
# Trophic State Index (Lamparelli) - implementacao base
# (ASCII-only no codigo)

#' Classifica TSI (Lamparelli) em faixas qualitativas
#'
#' @description
#' Converte valores do indice trofico de Lamparelli (TSI/IET) para classes
#' qualitativas ordenadas. Retorna fator ordenado em portugues ("pt")
#' ou ingles ("en").
#'
#' @param x Vetor numerico com TSI/IET (0-100). NA preservado.
#' @param locale Idioma dos rotulos: \code{"pt"} (padrao) ou \code{"en"}.
#'
#' @return Fator ordenado com classes de trofia (Lamparelli).
#'
#' @examples
#' classify_tsi_lamparelli(c(40, 50, 56, 61, 65, 72))
#' classify_tsi_lamparelli(c(40, 50, 56, 61, 65, 72), locale = "en")
#'
#' @export
classify_tsi_lamparelli <- function(x, locale = c("pt", "en")) {
  locale <- match.arg(locale)
  # Lamparelli: <47 Ultraoligo; 47-52 Oligo; 52-59 Meso; 59-63 Eutro; 63-67 Supereutro; >67 Hipereutro
  breaks <- c(-Inf, 47, 52, 59, 63, 67, Inf)

  if (locale == "pt") {
    labs <- c("Ultraoligotrofico", "Oligotrofico", "Mesotrofico",
              "Eutrofico", "Supereutrofico", "Hipereutrofico")
  } else {
    labs <- c("Ultra-oligotrophic", "Oligotrophic", "Mesotrophic",
              "Eutrophic", "Supereutrophic", "Hypereutrophic")
  }

  cut(x, breaks = breaks, labels = labs, right = TRUE, ordered_result = TRUE)
}

#' Trophic State Index (Lamparelli)
#'
#' @description
#' Computa componentes do indice trofico de Lamparelli (TSI/IET) a partir
#' de fosforo total, clorofila-a e profundidade do disco de Secchi, e
#' retorna o indice agregado como a media por linha dos componentes
#' disponiveis.
#'
#' Pode receber um \code{data.frame} como primeiro argumento (ver Detalhes).
#'
#' @param tp Fosforo total (mg/L) **ou** um data.frame contendo colunas
#'   \code{tp} (ug/L) ou \code{p_total} (mg/L), \code{chla} ou \code{clorofila}
#'   (ug/L), e \code{sd} ou \code{secchi} (m). Se for data.frame, \code{chla}
#'   e \code{sd} devem ser \code{NULL}.
#' @param chla Clorofila-a (ug/L).
#' @param sd Profundidade do disco de Secchi (m).
#' @param ambiente Tipo de ambiente: \code{"rio"} ou \code{"reservatorio"}.
#' @param .keep_ids Logico; quando data.frame, vincula colunas de ID
#'   (\code{rio}, \code{ponto}, \code{data}, \code{lat}, \code{lon}).
#'   Padrao \code{FALSE}.
#' @param add_status Logico; se \code{TRUE} (padrao), adiciona a coluna
#'   \code{TSI_status} com a classificacao qualitativa (Lamparelli).
#' @param locale Idioma de \code{TSI_status}: \code{"pt"} (padrao) ou \code{"en"}.
#' @param ... Reservado para uso futuro (ignorado).
#'
#' @details
#' Implementacao pragmatica; confirme coeficientes/limiares para seu
#' contexto regulatorio. Entradas com virgula decimal (ex.: "3,2") ou
#' desigualdades (ex.: "<0,1") sao convertidas com seguranca por
#' helpers internos. Se houver apenas \code{p_total} (mg/L), e convertida
#' para \code{tp} (ug/L) via \code{tp = p_total * 1000}.
#'
#' Os componentes e o indice agregado sao limitados ao intervalo \code{[0, 100]}
#' para consistencia com as figuras e tabelas do pacote/artigo.
#'
#' @returns
#' Um data.frame com colunas (quando aplicavel):
#' \itemize{
#'   \item \code{IET_TP} — componente de fosforo total (0-100).
#'   \item \code{IET_Chla} — componente de clorofila-a (0-100).
#'   \item \code{IET_Secchi} — componente de Secchi (0-100).
#'   \item \code{IET_Lamp} — indice Lamparelli agregado (0-100).
#'   \item \code{TSI_status} — classe qualitativa (quando \code{add_status=TRUE}).
#'   \item \code{ambiente} — tipo de ambiente informado.
#' }
#'
#' @seealso \code{\link[=iet_carlson]{iet_carlson()}}, \code{\link[=iqa]{iqa()}},
#'   \code{\link[=conama_check]{conama_check()}}
#'
#' @export
iet_lamparelli <- function(tp = NULL, chla = NULL, sd = NULL,
                           ambiente = c("rio", "reservatorio"),
                           .keep_ids = FALSE,
                           add_status = TRUE,
                           locale = c("pt", "en"),
                           ...) {
  ambiente <- match.arg(ambiente)
  locale <- match.arg(locale)

  clip01 <- function(x) pmin(100, pmax(0, x))

  # --- MODO DATA.FRAME (opcional): tp eh DF e demais NULL ---
  if (is.data.frame(tp) && is.null(chla) && is.null(sd)) {
    ext <- .df_extract_iet(tp)  # supoe helper interno existente

    res <- list()
    if (!is.null(ext$tp))        res$IET_TP     <- 10 + 10 * log10(pmax(ext$tp,        0.001))
    if (!is.null(ext$clorofila)) res$IET_Chla   <- 10 + 10 * log10(pmax(ext$clorofila, 0.001))
    if (!is.null(ext$secchi))    res$IET_Secchi <- 60 - 14.41 * log10(pmax(ext$secchi,  0.001))

    df_out <- as.data.frame(res, optional = TRUE)

    # clip de componentes
    for (nm in intersect(names(df_out), c("IET_TP","IET_Chla","IET_Secchi"))) {
      df_out[[nm]] <- clip01(df_out[[nm]])
    }

    # agregado
    if (ncol(df_out) > 1) {
      df_out$IET_Lamp <- rowMeans(df_out, na.rm = TRUE)
    } else if (ncol(df_out) == 1) {
      df_out$IET_Lamp <- df_out[[1]]
    } else {
      df_out <- data.frame(IET_Lamp = numeric(0))
    }
    df_out$IET_Lamp <- clip01(df_out$IET_Lamp)

    if (isTRUE(add_status) && nrow(df_out)) {
      df_out$TSI_status <- classify_tsi_lamparelli(df_out$IET_Lamp, locale = locale)
    }

    df_out$ambiente <- ambiente
    if (.keep_ids && nrow(ext$ids)) df_out <- cbind(ext$ids, df_out)

    attr(df_out, "iet_method") <- "Lamparelli"
    return(df_out)
  }

  # --- MODO VETORIAL (comportamento original) ---
  tp   <- .numify(tp)      # mg/L
  chla <- .numify(chla)    # ug/L
  sd   <- .numify(sd)      # m

  res <- list()
  if (!is.null(tp))   res$IET_TP     <- 10 + 10 * log10(pmax(tp,   0.001))
  if (!is.null(chla)) res$IET_Chla   <- 10 + 10 * log10(pmax(chla, 0.001))
  if (!is.null(sd))   res$IET_Secchi <- 60 - 14.41 * log10(pmax(sd,  0.001))

  df <- as.data.frame(res, optional = TRUE)

  for (nm in intersect(names(df), c("IET_TP","IET_Chla","IET_Secchi"))) {
    df[[nm]] <- clip01(df[[nm]])
  }

  if (ncol(df) > 1) {
    df$IET_Lamp <- rowMeans(df, na.rm = TRUE)
  } else if (ncol(df) == 1) {
    df$IET_Lamp <- df[[1]]
  } else {
    df <- data.frame(IET_Lamp = numeric(0))
  }
  df$IET_Lamp <- clip01(df$IET_Lamp)

  if (isTRUE(add_status) && nrow(df)) {
    df$TSI_status <- classify_tsi_lamparelli(df$IET_Lamp, locale = locale)
  }

  df$ambiente <- ambiente
  attr(df, "iet_method") <- "Lamparelli"
  df
}

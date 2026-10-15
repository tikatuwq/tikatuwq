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

# Equacoes de Lamparelli (2004) adotadas pela CETESB (Apendice D 2023, p. 10).
# tp e chla em ug/L (conferido pelas Tabelas 5 e 6: tp = 13 ug/L em rio da
# IET = 47.3, o corte Ultraoligotrofico/Oligotrofico).
.lamp_iet_pt <- function(tp_ug, ambiente) {
  if (ambiente == "rio") 10 * (6 - ((0.42 - 0.36 * log(tp_ug)) / log(2))) - 20
  else                   10 * (6 - ((1.77 - 0.42 * log(tp_ug)) / log(2)))
}
.lamp_iet_cl <- function(chla_ug, ambiente) {
  if (ambiente == "rio") 10 * (6 - ((-0.7 - 0.6 * log(chla_ug)) / log(2))) - 20
  else                   10 * (6 - ((0.92 - 0.34 * log(chla_ug)) / log(2)))
}
# Transparencia: a CETESB nao a usa no IET (Apendice D 2023, p. 9-10). O
# componente e calculado como auxiliar pela definicao de Carlson e NAO entra
# no indice agregado.
.lamp_iet_sd <- function(sd) 10 * (6 - log(sd) / log(2))

# Monta componentes e indice agregado de Lamparelli. tp_ug e chla em ug/L.
# IET = media de IET(PT) e IET(CL) disponiveis (Apendice D 2023, p. 10).
.lamp_assemble <- function(tp_ug, chla, sd, ambiente, clip01) {
  res <- list()
  if (!is.null(tp_ug)) res$IET_TP     <- clip01(.lamp_iet_pt(pmax(tp_ug, 0.001), ambiente))
  if (!is.null(chla))  res$IET_Chla   <- clip01(.lamp_iet_cl(pmax(chla, 0.001), ambiente))
  if (!is.null(sd))    res$IET_Secchi <- clip01(.lamp_iet_sd(pmax(sd, 0.001)))
  df <- as.data.frame(res, optional = TRUE)
  agg_cols <- intersect(names(df), c("IET_TP", "IET_Chla"))
  if (length(agg_cols) == 0L) {
    if (!is.null(sd)) {
      warning("Only Secchi depth supplied: CETESB's Lamparelli IET does not use transparency; IET_Lamp is NA.", call. = FALSE)
      df$IET_Lamp <- rep(NA_real_, nrow(df))
    } else {
      df <- data.frame(IET_Lamp = numeric(0))
    }
  } else {
    df$IET_Lamp <- rowMeans(df[, agg_cols, drop = FALSE], na.rm = TRUE)
    df$IET_Lamp[is.nan(df$IET_Lamp)] <- NA_real_
  }
  df
}

#' Trophic State Index (Lamparelli)
#'
#' @description
#' Computa o Indice do Estado Trofico de Lamparelli (2004), como adotado pela
#' CETESB, a partir de fosforo total e clorofila-a. O indice agregado e a media
#' de IET(PT) e IET(CL) disponiveis na linha.
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
#' Equacoes (CETESB 2023, Apendice D, secao 4, p. 10; PT e CL em ug/L,
#' ln = logaritmo natural):
#' \itemize{
#'   \item Rios: \code{IET(CL) = 10 (6 - ((-0.7 - 0.6 ln CL) / ln 2)) - 20};
#'     \code{IET(PT) = 10 (6 - ((0.42 - 0.36 ln PT) / ln 2)) - 20}.
#'   \item Reservatorios: \code{IET(CL) = 10 (6 - ((0.92 - 0.34 ln CL) / ln 2))};
#'     \code{IET(PT) = 10 (6 - ((1.77 - 0.42 ln PT) / ln 2))}.
#' }
#' Conferidas contra os cortes das Tabelas 5 e 6 da mesma fonte. A CETESB nao
#' usa a transparencia no IET; quando \code{sd} e informado, \code{IET_Secchi}
#' e devolvido como componente auxiliar (definicao de Carlson,
#' \code{10 (6 - ln SD / ln 2)}) e nao entra em \code{IET_Lamp}.
#' Ate a v0.10.0 a funcao usava \code{10 + 10 log10(x)} para PT e CL, igual
#' para rio e reservatorio, o que classificava praticamente qualquer amostra
#' como ultraoligotrofica.
#'
#' Entradas com virgula decimal (ex.: "3,2") ou
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
#'   \item \code{IET_Secchi} — componente auxiliar de Secchi (0-100), fora do agregado.
#'   \item \code{IET_Lamp} — indice Lamparelli agregado: media de IET_TP e IET_Chla (0-100).
#'   \item \code{TSI_status} — classe qualitativa (quando \code{add_status=TRUE}).
#'   \item \code{ambiente} — tipo de ambiente informado.
#' }
#'
#' @references
#' Lamparelli, M. C. (2004). Grau de trofia em corpos d'agua do Estado de Sao
#' Paulo: avaliacao dos metodos de monitoramento. Tese (Doutorado), Instituto de
#' Biociencias, Universidade de Sao Paulo.
#'
#' CETESB (2023). Apendice D: Indices de Qualidade das Aguas e Criterios de
#' Avaliacao da Qualidade dos Sedimentos. In: Qualidade das Aguas Interiores no
#' Estado de Sao Paulo. Secao 4, pp. 9-11.
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

    df_out <- .lamp_assemble(ext$tp, ext$clorofila, ext$secchi, ambiente, clip01)

    if (isTRUE(add_status) && nrow(df_out)) {
      df_out$TSI_status <- classify_tsi_lamparelli(df_out$IET_Lamp, locale = locale)
    }

    df_out$ambiente <- ambiente
    if (.keep_ids && nrow(ext$ids)) df_out <- cbind(ext$ids, df_out)

    attr(df_out, "iet_method") <- "Lamparelli"
    return(df_out)
  }

  # --- MODO VETORIAL (comportamento original) ---
  tp   <- .numify(tp)      # mg/L (convertido para ug/L abaixo)
  chla <- .numify(chla)    # ug/L
  sd   <- .numify(sd)      # m

  df <- .lamp_assemble(if (is.null(tp)) NULL else 1000 * tp, chla, sd, ambiente, clip01)

  if (isTRUE(add_status) && nrow(df)) {
    df$TSI_status <- classify_tsi_lamparelli(df$IET_Lamp, locale = locale)
  }

  df$ambiente <- ambiente
  attr(df, "iet_method") <- "Lamparelli"
  df
}

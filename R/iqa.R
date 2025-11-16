# R/iqa.R
# Water Quality Index (IQA/WQI) - implementacao com curvas aproximadas
# (ASCII-only no codigo)

#' Classifica valores do IQA/WQI em faixas qualitativas
#'
#' @description
#' Converte valores numericos de IQA (0-100) em classes qualitativas
#' padronizadas. Suporta rotulos em portugues ("pt") ou ingles ("en").
#'
#' @param x Vetor numerico com IQA em 0-100. Valores NA sao preservados.
#' @param locale Idioma dos rotulos: \code{"pt"} (padrao) ou \code{"en"}.
#'
#' @return Um fator ordenado com os rotulos de classe.
#'
#' @examples
#' classify_iqa(c(15, 40, 65, 80, 95))
#' classify_iqa(c(15, 40, 65, 80, 95), locale = "en")
#'
#' @export
classify_iqa <- function(x, locale = c("pt", "en")) {
  locale <- match.arg(locale)
  # Quebras oficiais (0-25, 26-50, 51-70, 71-90, 91-100)
  breaks <- c(-Inf, 25, 50, 70, 90, Inf)

  if (locale == "pt") {
    labs <- c("Muito ruim", "Ruim", "Regular", "Boa", "Otima")
  } else {
    labs <- c("Very Poor", "Poor", "Fair", "Good", "Excellent")
  }

  out <- cut(x, breaks = breaks, labels = labs, right = TRUE, ordered_result = TRUE)
  out
}

#' Water Quality Index (WQI / IQA)
#'
#' @description
#' Computa o IQA/WQI combinando subindices (Qi) por **media ponderada**.
#' Os subindices sao obtidos por interpolacao linear por trechos sobre
#' curvas aproximadas (estilo CETESB/NSF).
#'
#' @param df Data frame (ou tibble) com as colunas requeridas.
#'   Nomes esperados (portugues): \code{od}, \code{coliformes},
#'   \code{dbo}, \code{nt_total}, \code{p_total}, \code{turbidez},
#'   \code{tds}, \code{ph} (ou \code{pH}), \code{temperatura}.
#' @param pesos Pesos nomeados para cada parametro. Padroes seguem pratica
#'   CETESB/NSF: \code{od=.17}, \code{coliformes=.15}, \code{dbo=.10},
#'   \code{nt_total=.10}, \code{p_total=.10}, \code{turbidez=.08},
#'   \code{tds=.08}, \code{pH=.12}, \code{temperatura=.10}.
#' @param method Conjunto de curvas de interpolacao; atualmente apenas
#'   \code{"CETESB_approx"}.
#' @param na_rm Logico; se \code{FALSE} (padrao), linhas com Qi ausentes
#'   geram erro. Se \code{TRUE}, o IQA e computado usando apenas os
#'   parametros disponiveis e o denominador e ajustado para a soma dos
#'   pesos presentes.
#' @param add_status Logico; se \code{TRUE} (padrao), adiciona a coluna
#'   \code{IQA_status} com a classificacao qualitativa (0-100).
#' @param locale Idioma de \code{IQA_status}: \code{"pt"} (padrao) ou
#'   \code{"en"}.
#' @param ... Reservado para uso futuro (ignorado).
#'
#' @returns
#' O \code{df} de entrada com a coluna numerica \code{IQA} (0-100) e,
#' quando \code{add_status = TRUE}, a coluna fator \code{IQA_status}.
#' O atributo \code{"iqa_method"} e definido no objeto retornado.
#'
#' @details
#' Compatibilidade de nomes:
#' \itemize{
#'   \item A tabela de curvas usa a chave \code{"pH"}.
#'         Se seus dados possuem \code{ph} (minusculo), a curva \code{"pH"}
#'         e mapeada para a coluna \code{ph}.
#'   \item Para \code{temperatura}, a coluna \code{temp} (alias comum)
#'         e automaticamente aceita caso \code{temperatura} nao exista.
#' }
#'
#' Se as curvas internas retornarem Qi em 0-10 (variante historica),
#' o valor agregado e normalizado internamente para 0-100 antes do retorno.
#' Valores finais sao limitados ao intervalo \code{[0, 100]}.
#'
#' @examples
#' d <- wq_demo
#' d2 <- iqa(d, na_rm = TRUE)
#' table(d2$IQA_status, useNA = "ifany")
#'
#' @export
iqa <- function(
  df,
  pesos = c(
    od = .17, coliformes = .15, dbo = .10, nt_total = .10, p_total = .10,
    turbidez = .08, tds = .08, pH = .12, temperatura = .10
  ),
  method = c("CETESB_approx"),
  na_rm = FALSE,
  add_status = TRUE,
  locale = c("pt", "en"),
  ...
) {
  method <- match.arg(method)
  locale <- match.arg(locale)

  # Curvas (chaves: nomes dos parametros nas curvas; ex.: "pH")
  curves <- iqa_curve_table(method = method)

  # Helper de mapeamento de nome de curva -> coluna do df
  map_param_to_col <- function(param_name) {
    if (param_name == "pH" && "ph" %in% names(df)) return("ph")
    if (param_name == "temperatura" && "temp" %in% names(df)) return("temp")
    param_name
  }

  # Verificacao de colunas requeridas (com suporte a na_rm)
  req_curve_keys <- names(pesos)
  req_df_cols    <- vapply(req_curve_keys, map_param_to_col, character(1))

  present <- req_df_cols %in% names(df)
  if (!all(present)) {
    if (!na_rm) {
      missing_cols <- req_df_cols[!present]
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    } else {
      # Se na_rm = TRUE, reescala usando apenas as colunas presentes
      req_curve_keys <- req_curve_keys[present]
      req_df_cols    <- req_df_cols[present]
      pesos          <- pesos[names(pesos) %in% req_curve_keys]
    }
  }

  # Construcao de Qi por parametro (numificando antes da interpolacao)
  qi_col <- function(param_key) {
    col_name <- map_param_to_col(param_key)
    vals <- .numify(df[[col_name]])
    tbl  <- curves[[param_key]]
    if (is.null(tbl)) {
      stop("No Qi curve found for parameter key '", param_key, "'.")
    }
    qi_interp(vals, tbl)
  }

  qi_df <- as.data.frame(lapply(req_curve_keys, qi_col))
  names(qi_df) <- req_curve_keys

  # Se na_rm = FALSE e houver NA em Qi, aborta (comportamento anterior preservado)
  if (!na_rm && anyNA(qi_df)) {
    stop("There are NA values in parameters. Use na_rm = TRUE to ignore incomplete rows.")
  }

  # Pesos
  w_vec <- unname(pesos)
  # denominador por linha (soma de pesos onde Qi nao eh NA)
  denom <- rowSums(!is.na(qi_df) * rep(w_vec, each = nrow(qi_df)))
  # numerador: soma ponderada dos Qi
  numer <- rowSums(qi_df * matrix(rep(w_vec, each = nrow(qi_df)), nrow = nrow(qi_df)), na.rm = na_rm)

  iqa_val <- numer / denom
  iqa_val[denom == 0] <- NA_real_

  # Normaliza escala: se parecer 0-10, converte para 0-100
  # (detector simples e silencioso para compat)
  rng <- range(iqa_val, na.rm = TRUE)
  if (is.finite(rng[2]) && rng[2] <= 10) {
    iqa_val <- iqa_val * 10
  }

  # clip para [0, 100]
  iqa_val <- pmin(100, pmax(0, iqa_val))

  df$IQA <- iqa_val
  if (isTRUE(add_status)) {
    df$IQA_status <- classify_iqa(df$IQA, locale = locale)
  }

  attr(df, "iqa_method") <- method
  df
}

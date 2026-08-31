# R/iqa.R
# Water Quality Index (IQA/WQI) - media geometrica ponderada (CETESB/NSF)
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
  breaks <- c(-Inf, 25, 50, 70, 90, Inf)
  if (locale == "pt") {
    labs <- c("Muito ruim", "Ruim", "Regular", "Boa", "Otima")
  } else {
    labs <- c("Very Poor", "Poor", "Fair", "Good", "Excellent")
  }
  cut(x, breaks = breaks, labels = labs, right = TRUE, ordered_result = TRUE)
}

#' Water Quality Index (WQI / IQA)
#'
#' @description
#' Computa o IQA/WQI combinando subindices (Qi) por **media geometrica
#' ponderada**, conforme a metodologia oficial CETESB e o NSF WQI original
#' (Brown et al., 1970): \eqn{IQA = \prod_{i} Qi_i^{Wi}}{IQA = prod(Qi^Wi)}.
#'
#' @param df Data frame (ou tibble) com as colunas requeridas.
#'   Nomes esperados: \code{od}, \code{coliformes}, \code{dbo},
#'   \code{nt_total}, \code{p_total}, \code{turbidez}, \code{tds},
#'   \code{ph} (ou \code{pH}), \code{temperatura} (ou \code{temp}).
#' @param pesos Pesos nomeados para cada parametro. Padroes seguem pratica
#'   CETESB/NSF: \code{od=.17}, \code{coliformes=.15}, \code{dbo=.10},
#'   \code{nt_total=.10}, \code{p_total=.10}, \code{turbidez=.08},
#'   \code{tds=.08}, \code{pH=.12}, \code{temperatura=.10}.
#' @param method Metodo de calculo:
#'   \itemize{
#'     \item \code{"CETESB"} (padrao) — subindices por curvas de interpolacao
#'           + **media geometrica ponderada**.
#'     \item \code{"CETESB_equations"} — equacoes polinomiais CETESB com
#'           saturacao de OD dependente de temperatura e altitude +
#'           media geometrica ponderada.
#'     \item \code{"NSF_approx"} — subindices por curvas + media aritmetica
#'           ponderada (metodo legado, mantido para compatibilidade).
#'   }
#' @param altitude_m Altitude em metros acima do nivel do mar (default \code{0}).
#'   Usado apenas em \code{method = "CETESB_equations"} para correcao da
#'   saturacao de oxigenio dissolvido.
#' @param na_rm Logico; se \code{FALSE} (padrao), linhas com Qi ausentes
#'   geram erro. Se \code{TRUE}, o IQA e calculado com os parametros
#'   disponiveis e os pesos sao renormalizados por linha.
#' @param add_status Logico; se \code{TRUE} (padrao), adiciona a coluna
#'   \code{IQA_status} com a classificacao qualitativa (0-100).
#' @param locale Idioma de \code{IQA_status}: \code{"pt"} (padrao) ou
#'   \code{"en"}.
#' @param ... Reservado para uso futuro.
#'
#' @returns
#' O \code{df} de entrada com a coluna numerica \code{IQA} (0-100) e,
#' quando \code{add_status = TRUE}, a coluna fator \code{IQA_status}.
#' O atributo \code{"iqa_method"} e definido no objeto retornado.
#'
#' @details
#' **Metodo de agregacao (correcao em v0.9.0):**
#' O IQA CETESB e o NSF WQI original utilizam media geometrica ponderada.
#' O metodo \code{"NSF_approx"} (media aritmetica) e mantido apenas para
#' compatibilidade retroativa.
#'
#' **Compatibilidade de nomes de coluna:**
#' \itemize{
#'   \item \code{ph} (minusculo) e aceito como alias de \code{pH}.
#'   \item \code{temp} e aceito como alias de \code{temperatura}.
#' }
#'
#' @references
#' CETESB (2021). \emph{Qualidade das Aguas Superficiais no Estado de Sao Paulo}.
#' CETESB, Sao Paulo.
#'
#' Brown, R.M. et al. (1970). A Water Quality Index — Do We Dare?
#' \emph{Water and Sewage Works}, 117, 339-343.
#'
#' @family water-quality-indices
#'
#' @examples
#' d <- iqa(wq_demo, na_rm = TRUE)
#' table(d$IQA_status, useNA = "ifany")
#'
#' # Usando equacoes CETESB com correcao de altitude
#' \donttest{
#' d2 <- iqa(wq_demo, method = "CETESB_equations", altitude_m = 800, na_rm = TRUE)
#' summary(d2$IQA)
#' }
#'
#' @export
iqa <- function(
  df,
  pesos = c(
    od = .17, coliformes = .15, dbo = .10, nt_total = .10, p_total = .10,
    turbidez = .08, tds = .08, pH = .12, temperatura = .10
  ),
  method    = c("CETESB", "CETESB_equations", "NSF_approx"),
  altitude_m = 0,
  na_rm     = FALSE,
  add_status = TRUE,
  locale    = c("pt", "en"),
  ...
) {
  method <- match.arg(method)
  locale <- match.arg(locale)

  # ------------------------------------------------------------------
  # Rota: equacoes polinomiais CETESB (iqa_official)
  # ------------------------------------------------------------------
  if (method == "CETESB_equations") {
    out <- iqa_official(df, pesos = pesos, altitude_m = altitude_m, na_rm = na_rm)
    out$IQA <- pmin(100, pmax(0, out$IQA))
    if (isTRUE(add_status)) {
      out$IQA_status <- classify_iqa(out$IQA, locale = locale)
    }
    attr(out, "iqa_method") <- method
    return(out)
  }

  # ------------------------------------------------------------------
  # Rotas: CETESB (geometrica) e NSF_approx (aritmetica) via curvas
  # ------------------------------------------------------------------
  curves <- iqa_curve_table(method = "CETESB_approx")

  map_param_to_col <- function(param_name) {
    if (param_name == "pH"         && "ph"   %in% names(df)) return("ph")
    if (param_name == "temperatura" && "temp" %in% names(df)) return("temp")
    param_name
  }

  req_curve_keys <- names(pesos)
  req_df_cols    <- vapply(req_curve_keys, map_param_to_col, character(1))
  present        <- req_df_cols %in% names(df)

  if (!all(present)) {
    if (!na_rm) {
      stop("Missing required columns: ", paste(req_df_cols[!present], collapse = ", "))
    }
    req_curve_keys <- req_curve_keys[present]
    req_df_cols    <- req_df_cols[present]
    pesos          <- pesos[names(pesos) %in% req_curve_keys]
  }

  qi_col <- function(param_key) {
    col_name <- map_param_to_col(param_key)
    vals     <- .numify(df[[col_name]])
    tbl      <- curves[[param_key]]
    if (is.null(tbl)) stop("No Qi curve found for parameter key '", param_key, "'.")
    qi_interp(vals, tbl)
  }

  qi_df <- as.data.frame(lapply(req_curve_keys, qi_col))
  names(qi_df) <- req_curve_keys

  if (!na_rm && anyNA(qi_df)) {
    stop("There are NA values in parameters. Use na_rm = TRUE to ignore incomplete rows.")
  }

  w_vec <- unname(pesos)

  # ------------------------------------------------------------------
  # Agregacao: geometrica (CETESB, padrao) ou aritmetica (NSF_approx)
  # ------------------------------------------------------------------
  if (method == "CETESB") {
    # Media geometrica ponderada: IQA = prod(Qi^Wi)
    iqa_val <- vapply(seq_len(nrow(qi_df)), function(i) {
      qi_row <- unlist(qi_df[i, , drop = TRUE])
      ok     <- is.finite(qi_row)
      if (!any(ok)) return(NA_real_)
      ww <- w_vec[ok]
      if (na_rm) ww <- ww / sum(ww) else ww <- ww / sum(w_vec)
      prod(qi_row[ok]^ww)
    }, numeric(1))
  } else {
    # NSF_approx: media aritmetica ponderada (legado)
    denom   <- rowSums(!is.na(qi_df) * matrix(rep(w_vec, each = nrow(qi_df)),
                                               nrow = nrow(qi_df)))
    numer   <- rowSums(qi_df * matrix(rep(w_vec, each = nrow(qi_df)),
                                      nrow = nrow(qi_df)), na.rm = na_rm)
    iqa_val <- ifelse(denom == 0, NA_real_, numer / denom)
  }

  # Normaliza escala se necessario (0-10 -> 0-100, variante historica)
  rng <- range(iqa_val, na.rm = TRUE)
  if (is.finite(rng[2]) && rng[2] <= 10) iqa_val <- iqa_val * 10

  iqa_val <- pmin(100, pmax(0, iqa_val))

  df$IQA <- iqa_val
  if (isTRUE(add_status)) df$IQA_status <- classify_iqa(df$IQA, locale = locale)

  attr(df, "iqa_method") <- method
  df
}

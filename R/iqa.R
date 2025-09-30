# R/iqa.R
# Water Quality Index (IQA/WQI) - implementacao com curvas aproximadas
# (ASCII-only no codigo)

#' Water Quality Index (WQI / IQA)
#'
#' @description
#' Computes IQA/WQI by combining parameter-specific sub-scores (Qi) via
#' a **weighted mean**. Sub-scores are obtained by piecewise-linear
#' interpolation over approximate curves (CETESB/NSF-like).
#'
#' @param df Data frame (or tibble) with required parameter columns.
#'   Expected defaults (Portuguese names): \code{od}, \code{coliformes},
#'   \code{dbo}, \code{nt_total}, \code{p_total}, \code{turbidez},
#'   \code{tds}, \code{ph} (or \code{pH}), \code{temperatura}.
#' @param pesos Named numeric weights for each parameter (sum not required).
#'   Defaults follow CETESB/NSF practice:
#'   \itemize{
#'     \item \code{od = 0.17}
#'     \item \code{coliformes = 0.15}
#'     \item \code{dbo = 0.10}
#'     \item \code{nt_total = 0.10}
#'     \item \code{p_total = 0.10}
#'     \item \code{turbidez = 0.08}
#'     \item \code{tds = 0.08}
#'     \item \code{pH = 0.12} (mapped to column \code{ph} if needed)
#'     \item \code{temperatura = 0.10}
#'   }
#' @param method Character scalar; interpolation table set. Currently
#'   only \code{"CETESB_approx"}.
#' @param na_rm Logical; if \code{FALSE} (default), rows containing
#'   missing Qi values will trigger an error. If \code{TRUE}, the IQA
#'   is computed using only available parameters, with the denominator
#'   adjusted to the sum of the weights of present parameters.
#'
#' @returns
#' The input \code{df} with an added numeric column \code{IQA}. The
#' attribute \code{"iqa_method"} is set on the returned data.frame/tibble.
#'
#' @details
#' Column name compatibility:
#' \itemize{
#'   \item The interpolation table uses the key \code{"pH"}.
#'         If your data uses a \code{ph} column (lowercase), it is
#'         automatically mapped to the \code{"pH"} curve.
#'   \item All other parameter names are used as-is.
#' }
#'
#' Values are clipped into \code{[0, 100]} after aggregation.
#'
#' @examples
#' # Minimal example using the demo data:
#' d <- wq_demo
#' d2 <- iqa(d, na_rm = TRUE)
#' head(d2$IQA)
#'
#' @export
iqa <- function(
  df,
  pesos = c(
    od = .17, coliformes = .15, dbo = .10, nt_total = .10, p_total = .10,
    turbidez = .08, tds = .08, pH = .12, temperatura = .10
  ),
  method = c("CETESB_approx"),
  na_rm = FALSE
) {
  method <- match.arg(method)

  # Curvas (chaves: nomes dos parametros nas curvas; ex.: "pH")
  curves <- iqa_curve_table(method = method)

  # Mapeia nomes dos pesos (chaves esperadas de curvas) para colunas do df
  # Tratamento especial: "pH" (curva) pode estar como "ph" (coluna do df)
  map_param_to_col <- function(param_name) {
    if (param_name == "pH" && "ph" %in% names(df)) return("ph")
    param_name
  }

  # Verificacao de colunas requeridas
  req_curve_keys <- names(pesos)
  req_df_cols    <- vapply(req_curve_keys, map_param_to_col, character(1))
  missing_cols   <- setdiff(req_df_cols, names(df))
  if (length(missing_cols)) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Construcao de Qi por parametro
  qi_col <- function(param_key) {
    col_name <- map_param_to_col(param_key)
    vals <- df[[col_name]]
    tbl  <- curves[[param_key]]
    if (is.null(tbl)) {
      stop("No Qi curve found for parameter key '", param_key, "'.")
    }
    qi_interp(vals, tbl)
  }

  qi_df <- as.data.frame(lapply(req_curve_keys, qi_col))
  names(qi_df) <- req_curve_keys

  # Denominador: soma dos pesos presentes por linha
  # Se na_rm = FALSE e houver NA, aborta para evitar medias enviesadas
  if (!na_rm && anyNA(qi_df)) {
    stop("There are NA values in parameters. Use na_rm = TRUE to ignore incomplete rows.")
  }

  w_vec <- unname(pesos)
  # matriz de pesos por linha
  w_mat <- matrix(rep(w_vec, each = nrow(qi_df)), nrow = nrow(qi_df))
  # denominador por linha (soma de pesos onde Qi nao eh NA)
  denom <- rowSums(!is.na(qi_df) * rep(w_vec, each = nrow(qi_df)))
  # numerador: soma ponderada dos Qi
  numer <- rowSums(qi_df * w_mat, na.rm = na_rm)

  iqa_val <- numer / denom
  iqa_val[denom == 0] <- NA_real_  # se todos os Qi forem NA na linha
  # clip para [0, 100]
  iqa_val <- pmin(100, pmax(0, iqa_val))

  df$IQA <- iqa_val
  attr(df, "iqa_method") <- method
  df
}

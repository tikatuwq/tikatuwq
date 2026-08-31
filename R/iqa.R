# R/iqa.R
# Water Quality Index (IQA/WQI) - Metodologia Oficial CETESB validada
# (ASCII-only no codigo)

#' Classifica valores do IQA/WQI em faixas qualitativas
#'
#' @description
#' Converte valores numericos de IQA (0-100) em classes qualitativas
#' padronizadas. O padrao oficial CETESB utiliza as faixas:
#' Pessima (0-19), Ruim (19-36), Regular (36-51), Boa (51-79) e Otima (79-100).
#'
#' @param x Vetor numerico com IQA em 0-100. Valores NA sao preservados.
#' @param scheme Esquema de classificacao: \code{"cetesb"} (padrao oficial) ou \code{"legacy"} (escala 25/50/70/90).
#' @param locale Idioma dos rotulos: \code{"pt"} (padrao) ou \code{"en"}.
#'
#' @return Um fator ordenado com os rotulos de classe.
#'
#' @examples
#' classify_iqa(c(15, 30, 45, 70, 85))
#' classify_iqa(c(15, 30, 45, 70, 85), locale = "en")
#'
#' @export
classify_iqa <- function(x, scheme = c("cetesb", "legacy"), locale = c("pt", "en")) {
  scheme <- match.arg(scheme)
  locale <- match.arg(locale)

  if (scheme == "cetesb") {
    # Faixas oficiais CETESB: <=19, 19-36, 36-51, 51-79, >79
    breaks <- c(-Inf, 19, 36, 51, 79, Inf)
    if (locale == "pt") {
      labs <- c("Pessima", "Ruim", "Regular", "Boa", "Otima")
    } else {
      labs <- c("Very Poor", "Poor", "Fair", "Good", "Excellent")
    }
  } else {
    # Esquema legado aproximado
    breaks <- c(-Inf, 25, 50, 70, 90, Inf)
    if (locale == "pt") {
      labs <- c("Muito ruim", "Ruim", "Regular", "Boa", "Otima")
    } else {
      labs <- c("Very Poor", "Poor", "Fair", "Good", "Excellent")
    }
  }

  cut(x, breaks = breaks, labels = labs, right = TRUE, ordered_result = TRUE)
}

#' Water Quality Index (WQI / IQA CETESB Oficial)
#'
#' @description
#' Computa o IQA brasileiro oficial conforme a metodologia da CETESB/INEMA
#' utilizando equacoes analiticas continuas validadas e agregacao por
#' **media geometrica ponderada**: \eqn{IQA = \prod_{i=1}^{9} q_i^{w_i}}.
#'
#' @param df Data frame ou tibble com as colunas de monitoramento.
#'   Nomes canônicos reconhecidos: \code{od}, \code{solidos_totais} (ou \code{residuo_total}),
#'   \code{ph}, \code{dbo}, \code{coliformes_termotolerantes} (ou \code{coliformes}),
#'   \code{nt_total}, \code{p_total}, \code{turbidez}, \code{temperatura}.
#' @param method Metodo de calculo:
#'   \itemize{
#'     \item \code{"CETESB"} (padrao) — equacoes analiticas continuas oficiais CETESB/INEMA
#'           com media geometrica ponderada.
#'     \item \code{"CETESB_equations"} — alias de \code{"CETESB"}.
#'     \item \code{"CETESB_legacy_approx"} — subindices por curvas interpoladas aproximadas (legado).
#'     \item \code{"NSF_approx"} — subindices por curvas aproximadas com media aritmetica (legado).
#'   }
#' @param classification Esquema de classificacao qualitativa: \code{"cetesb"} (padrao: 19/36/51/79)
#'   ou \code{"legacy"} (25/50/70/90).
#' @param altitude_m Altitude em metros acima do nivel do mar (default \code{0}), usada no calculo
#'   da concentracao de saturacao de OD.
#' @param phosphorus_basis Base do fosforo informado: \code{"P"} (default, aplica fator 3.066 para PO4)
#'   ou \code{"PO4"}.
#' @param microbial_type Tipo microbiologico informado: \code{"thermotolerant_coliforms"} (default)
#'   ou \code{"e_coli"} (aplica fator 1.25x da CETESB).
#' @param temperature_method Metodo para o componente termico: \code{"cetesb_default"} (default: Qi=94),
#'   \code{"delta"} (usa delta_temperatura) ou \code{"reference"} (calcula temp_agua - temp_ref).
#' @param temperature_reference Temperatura de referencia quando \code{temperature_method = "reference"}.
#' @param delta_temperature Variacao de temperatura quando \code{temperature_method = "delta"}.
#' @param censor_policy Politica para dados censurados: \code{"limit"} (default: <X -> X),
#'   \code{"half_limit"} (<X -> X/2), \code{"zero"}, \code{"na"}, ou \code{"preserve"}.
#' @param allow_partial Logico; se \code{FALSE} (padrao estrito), exige todos os componentes presentes.
#'   Se \code{TRUE}, permite calculo parcial renormalizando pesos dos parametros disponiveis.
#' @param details Logico; se \code{TRUE}, anexa o dataframe detalhado de componentes no atributo \code{"components"}.
#' @param add_status Logico; se \code{TRUE} (padrao), adiciona a coluna \code{IQA_status}.
#' @param locale Idioma dos rotulos qualitativos: \code{"pt"} (padrao) ou \code{"en"}.
#' @param pesos Pesos customizados (opcional; padrao sao os 9 pesos oficiais CETESB).
#' @param ... Parametros adicionais reservados.
#'
#' @returns
#' O data frame de entrada com a coluna numerica \code{IQA} (0-100) e,
#' quando \code{add_status = TRUE}, a coluna fator \code{IQA_status}.
#'
#' @export
#' @examples
#' d <- data.frame(
#'   od = 6.5, temperatura = 25, dbo = 2, coliformes = 200,
#'   ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8,
#'   solidos_totais = 120
#' )
#' out <- iqa(d)
#' out$IQA
#' out$IQA_status
iqa <- function(
  df,
  method = c("CETESB", "CETESB_equations", "CETESB_legacy_approx", "NSF_approx"),
  classification = c("cetesb", "legacy"),
  altitude_m = 0,
  phosphorus_basis = c("P", "PO4"),
  microbial_type = c("thermotolerant_coliforms", "e_coli"),
  temperature_method = c("cetesb_default", "delta", "reference"),
  temperature_reference = NULL,
  delta_temperature = NULL,
  censor_policy = c("limit", "half_limit", "zero", "na", "preserve"),
  allow_partial = FALSE,
  details = FALSE,
  add_status = TRUE,
  locale = c("pt", "en"),
  pesos = NULL,
  ...
) {
  method         <- match.arg(method)
  classification <- match.arg(classification)
  phosphorus_basis <- match.arg(phosphorus_basis)
  microbial_type <- match.arg(microbial_type)
  temperature_method <- match.arg(temperature_method)
  censor_policy  <- match.arg(censor_policy)
  locale         <- match.arg(locale)

  dots <- list(...)
  if (!is.null(dots$na_rm) && isTRUE(dots$na_rm)) {
    allow_partial <- TRUE
  }

  if (nrow(df) == 0L) {
    out <- df
    out$IQA <- numeric(0)
    if (isTRUE(add_status)) out$IQA_status <- factor()
    return(out)
  }

  # ------------------------------------------------------------------
  # Rotas Oficiais CETESB (Analitica)
  # ------------------------------------------------------------------
  if (method %in% c("CETESB", "CETESB_equations")) {
    # Verificacao estrita: TDS NAO e Solidos Totais
    has_st  <- !is.null(.resolve_col(df, "solidos_totais"))
    has_tds <- !is.null(.resolve_col(df, "tds"))
    if (!allow_partial && !has_st && has_tds) {
      stop(
        "CETESB IQA requires total solids (solidos_totais/residuo_total). ",
        "TDS (total dissolved solids) is not interchangeable with total solids. ",
        "Please provide 'solidos_totais' or use allow_partial = TRUE if total solids was not measured."
      )
    }


    # Calculo dos componentes detalhados
    comps <- iqa_components(
      df = df,
      altitude_m = altitude_m,
      phosphorus_basis = phosphorus_basis,
      microbial_type = microbial_type,
      temperature_method = temperature_method,
      temperature_reference = temperature_reference,
      censor_policy = censor_policy
    )

    qi_cols <- c("qi_od", "qi_colif", "qi_ph", "qi_dbo", "qi_nt", "qi_pt", "qi_temp", "qi_turb", "qi_st")
    wi_cols <- c("wi_od", "wi_colif", "wi_ph", "wi_dbo", "wi_nt", "wi_pt", "wi_temp", "wi_turb", "wi_st")

    qi_mat <- as.matrix(comps[, qi_cols])
    wi_mat <- as.matrix(comps[, wi_cols])

    # Se pesos customizados foram fornecidos
    if (!is.null(pesos)) {
      w_vec <- rep(NA_real_, 9)
      names(w_vec) <- c("od", "coliformes_termotolerantes", "ph", "dbo", "nt_total", "p_total", "temperatura", "turbidez", "solidos_totais")
      for (p_nm in names(pesos)) {
        canon <- .alias_to_canonical[p_nm]
        if (!is.na(canon) && canon %in% names(w_vec)) w_vec[canon] <- pesos[[p_nm]]
      }
      for (k in seq_len(9)) {
        if (!is.na(w_vec[k])) wi_mat[, k] <- w_vec[k]
      }
    }

    # Calculo linha a linha
    IQA_vals <- rep(NA_real_, nrow(df))
    for (i in seq_len(nrow(df))) {
      q_row <- qi_mat[i, ]
      w_row <- wi_mat[i, ]
      ok <- is.finite(q_row) & is.finite(w_row)

      if (!allow_partial && !all(ok)) {
        IQA_vals[i] <- NA_real_
      } else if (any(ok)) {
        ww <- w_row[ok] / sum(w_row[ok])
        IQA_vals[i] <- prod(q_row[ok] ^ ww)
      }
    }

    out <- df
    out$IQA <- round(pmin(100, pmax(0, IQA_vals)), 2)

    if (isTRUE(add_status)) {
      out$IQA_status <- classify_iqa(out$IQA, scheme = classification, locale = locale)
    }

    if (isTRUE(details)) {
      attr(out, "components") <- comps
    }

    attr(out, "iqa_method")             <- "CETESB"
    attr(out, "iqa_classification")     <- classification
    attr(out, "iqa_censor_policy")      <- censor_policy
    attr(out, "iqa_phosphorus_basis")   <- phosphorus_basis
    attr(out, "iqa_microbial_type")     <- microbial_type
    attr(out, "iqa_temperature_method") <- temperature_method
    attr(out, "iqa_altitude_m")         <- altitude_m
    attr(out, "iqa_complete")           <- !allow_partial

    return(out)
  }

  # ------------------------------------------------------------------
  # Rotas Legadas via Curvas Aproximadas (CETESB_legacy_approx / NSF_approx)
  # ------------------------------------------------------------------
  curves <- iqa_curve_table(method = "CETESB_approx")

  map_param_to_col <- function(param_name) {
    if (param_name == "pH"         && "ph"   %in% names(df)) return("ph")
    if (param_name == "temperatura" && "temp" %in% names(df)) return("temp")
    param_name
  }

  if (is.null(pesos)) {
    pesos <- c(
      od = .17, coliformes = .15, dbo = .10, nt_total = .10, p_total = .10,
      turbidez = .08, tds = .08, pH = .12, temperatura = .10
    )
  }

  req_curve_keys <- names(pesos)
  req_df_cols    <- vapply(req_curve_keys, map_param_to_col, character(1))
  present        <- req_df_cols %in% names(df)

  if (!allow_partial && !all(present)) {
    missing <- req_curve_keys[!present]
    stop("Missing required parameters for legacy method: ", paste(missing, collapse = ", "))
  }

  w_full <- pesos
  qi_df  <- as.data.frame(matrix(NA_real_, nrow = nrow(df), ncol = length(pesos)))
  names(qi_df) <- names(pesos)

  for (i in seq_along(pesos)) {
    pname <- names(pesos)[i]
    col   <- req_df_cols[i]
    if (col %in% names(df)) {
      vals <- .parse_nd_ld(df[[col]], censor_policy)
      tbl  <- curves[[pname]]
      if (!is.null(tbl)) {
        qi_df[[pname]] <- stats::approx(
          x      = tbl$x,
          y      = tbl$y,
          xout   = vals,
          rule   = 2,
          ties   = "ordered"
        )$y
      }
    }
  }

  IQA_val <- vapply(seq_len(nrow(df)), function(row_idx) {
    qi_row <- unlist(qi_df[row_idx, ])
    ok     <- is.finite(qi_row)
    if (!any(ok)) return(NA_real_)
    if (!allow_partial && !all(ok)) return(NA_real_)

    ww <- w_full[ok] / sum(w_full[ok])
    if (method == "NSF_approx") {
      sum(qi_row[ok] * ww)
    } else {
      prod(qi_row[ok] ^ ww)
    }
  }, numeric(1))

  out <- df
  out$IQA <- round(pmin(100, pmax(0, IQA_val)), 2)

  if (isTRUE(add_status)) {
    out$IQA_status <- classify_iqa(out$IQA, scheme = classification, locale = locale)
  }

  attr(out, "iqa_method") <- method
  out
}

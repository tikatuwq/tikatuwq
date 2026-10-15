# R/iqa.R
# Water Quality Index (IQA/WQI) - Metodologia CETESB (equacoes em auditoria)
# (ASCII-only no codigo)

#' Classifica valores do IQA/WQI em faixas qualitativas
#'
#' @description
#' Converte valores numericos de IQA (0-100) em classes qualitativas conforme a
#' escala institucional escolhida. A classificacao nao altera o valor numerico:
#' ela apenas aplica cortes e rotulos a um IQA ja calculado.
#'
#' Escalas disponiveis (intervalos fechados a direita, ex.: Ruim = (19, 36]):
#' \describe{
#'   \item{\code{"cetesb"}}{CETESB/SP (padrao): Pessima \[0, 19\], Ruim (19, 36],
#'     Regular (36, 51], Boa (51, 79], Otima (79, 100] (Apendice D CETESB 2023,
#'     Tabela 1, p. 5).}
#'   \item{\code{"ana"}}{Portal de Qualidade das Aguas da ANA
#'     (qualidadedaagua.ana.gov.br/iqa.html, consultado em 2026-10-15), que
#'     apresenta os mesmos cortes 19/36/51/79 da CETESB. E, portanto, um alias
#'     de \code{"cetesb"}. Faixas historicas de outros estados (ex.: 20/37/52/79)
#'     citadas em publicacoes antigas da ANA nao estao implementadas.}
#'   \item{\code{"igam"}}{IGAM/MG: Muito Ruim \[0, 25\], Ruim (25, 50],
#'     Medio (50, 70], Bom (70, 90], Excelente (90, 100] (IGAM, relatorio de
#'     monitoramento da sub-bacia do ribeirao Pampulha, 3o trimestre de 2017,
#'     p. 9).}
#'   \item{\code{"legacy"}}{Escala aproximada mantida por compatibilidade com
#'     versoes antigas do pacote (cortes 25/50/70/90, rotulos Muito ruim/Ruim/
#'     Regular/Boa/Otima). Nao corresponde a nenhuma escala institucional verificada.}
#' }
#'
#' Valores \code{NA}/\code{NaN} resultam em \code{NA}. Valores fora de
#' \[0, 100\] (inclusive \code{Inf}) tambem resultam em \code{NA}, com aviso.
#' A classificacao e feita sobre o valor recebido, sem arredondamento adicional.
#'
#' @note Usar \code{classification = "igam"} sobre um IQA calculado pelo metodo
#'   CETESB nao equivale ao IQA calculado integralmente pelo IGAM, que usa
#'   parametros (nitrato, fosfato total), qi de temperatura (92) e regra de dados
#'   ausentes proprios.
#'
#' @param x Vetor numerico com IQA em 0-100.
#' @param classification Escala de classificacao: \code{"cetesb"} (padrao),
#'   \code{"igam"}, \code{"ana"} ou \code{"legacy"}.
#' @param locale Idioma dos rotulos: \code{"pt"} (padrao) ou \code{"en"}.
#' @param scheme Depreciado (desde 0.11.0); use \code{classification}. Se ambos
#'   forem informados com valores diferentes, ocorre erro.
#'
#' @return Um fator ordenado com os rotulos de classe.
#'
#' @examples
#' vals <- c(15, 28, 45, 65, 85, 95)
#'
#' # Classificacao CETESB (padrao)
#' classify_iqa(vals)
#'
#' # Classificacao IGAM (Minas Gerais)
#' classify_iqa(vals, classification = "igam")
#'
#' # Comparar escalas lado a lado
#' data.frame(
#'   IQA    = vals,
#'   CETESB = classify_iqa(vals, classification = "cetesb"),
#'   IGAM   = classify_iqa(vals, classification = "igam")
#' )
#'
#' @export
classify_iqa <- function(
  x,
  classification = c("cetesb", "igam", "ana", "legacy"),
  locale = c("pt", "en"),
  scheme = NULL
) {
  if (!is.null(scheme)) {
    scheme <- match.arg(scheme, c("cetesb", "igam", "ana", "legacy"))
    if (!missing(classification) && scheme != match.arg(classification, c("cetesb", "igam", "ana", "legacy"))) {
      stop("Conflicting arguments: 'scheme' and 'classification' differ. Use 'classification' only.")
    }
    warning("Argument 'scheme' is deprecated in classify_iqa(); use 'classification' instead.", call. = FALSE)
    classification <- scheme
  }
  classification <- match.arg(classification, c("cetesb", "igam", "ana", "legacy"))
  locale <- match.arg(locale)

  x <- as.numeric(x)
  out_of_range <- !is.na(x) & (x < 0 | x > 100)
  if (any(out_of_range)) {
    warning(sum(out_of_range), " IQA value(s) outside [0, 100] classified as NA.", call. = FALSE)
    x[out_of_range] <- NA_real_
  }

  en_labs <- c("Very Poor", "Poor", "Fair", "Good", "Excellent")

  if (classification %in% c("cetesb", "ana")) {
    # CETESB/SP; ANA (portal atual) adota os mesmos cortes
    breaks <- c(0, 19, 36, 51, 79, 100)
    labs <- if (locale == "pt") c("Pessima", "Ruim", "Regular", "Boa", "Otima") else en_labs

  } else if (classification == "igam") {
    # IGAM/MG; rotulos e cortes conforme relatorio IGAM 2017, p. 9
    breaks <- c(0, 25, 50, 70, 90, 100)
    labs <- if (locale == "pt") c("Muito Ruim", "Ruim", "Medio", "Bom", "Excelente") else en_labs

  } else {
    # Escala aproximada legada do pacote
    breaks <- c(0, 25, 50, 70, 90, 100)
    labs <- if (locale == "pt") c("Muito ruim", "Ruim", "Regular", "Boa", "Otima") else en_labs
  }

  cut(x, breaks = breaks, labels = labs, include.lowest = TRUE, right = TRUE, ordered_result = TRUE)
}

#' Water Quality Index (WQI / IQA CETESB)
#'
#' @description
#' Calcula o IQA segundo a metodologia CETESB (Apendice D, 2023): subindices
#' \eqn{q_i} agregados por **media geometrica ponderada**,
#' \eqn{IQA = \prod_{i=1}^{9} q_i^{w_i}}.
#'
#' A CETESB publica os \eqn{q_i} apenas como curvas graficas (Figura 1 do
#' Apendice D). O pacote usa equacoes analiticas por partes que aproximam essas
#' curvas; os pesos, os valores fixos fora do dominio (ex.: DBO > 30 -> 2,
#' OD > 140\% -> 47) e a classificacao conferem com a fonte. Na versao 0.11.0
#' os trechos de DBO (5-15 mg/L), nitrogenio total (10-60 mg/L) e pH (8,5-10)
#' foram corrigidos apos comparacao com as curvas. A curva de variacao de
#' temperatura (usada so com \code{temperature_method} diferente do padrao) nao
#' acompanha a curva oficial. Veja
#' \code{system.file("audit", "IQA_AUDITORIA.md", package = "tikatuwq")}.
#'
#' @section Metodo x classificacao:
#' \code{method} define o motor de calculo completo (parametros, curvas, pesos,
#' politica de ausentes) e a escala de classificacao correspondente:
#' \code{iqa(df)} calcula e classifica pela CETESB. Para aplicar outra escala a
#' um IQA ja calculado, use \code{\link{classify_iqa}}. O argumento
#' \code{classification} de \code{iqa()} esta depreciado desde 0.11.0: ainda
#' funciona, com aviso, e uma escala diferente da do metodo (ex.:
#' \code{iqa(df, classification = "igam")}) e registrada como classificacao
#' cruzada em \code{attr(, "iqa_nonstandard")}. Isso nao e um IQA IGAM.
#'
#' Plano de depreciacao: 0.11.0 aviso; remocao do argumento nao antes da 0.13.0,
#' anunciada no NEWS com uma versao de antecedencia.
#'
#' @section Dados ausentes:
#' Pela CETESB, sem algum dos nove parametros "o calculo do IQA e inviabilizado"
#' (Apendice D 2023, p. 4). Assim, com \code{allow_partial = FALSE} (padrao),
#' essas linhas recebem \code{NA}. Com \code{allow_partial = TRUE}, os pesos dos parametros
#' disponiveis sao renormalizados proporcionalmente (\eqn{w_i / \sum w_{disp}}).
#' Esta e uma estimativa parcial do pacote, nao uma regra institucional: o IGAM,
#' por exemplo, nao calcula o IQA sem OD ou coliformes e reparte o peso dos
#' demais ausentes igualmente. Linhas parciais ficam registradas no atributo
#' \code{"iqa_partial_rows"} e, com \code{details = TRUE}, nas colunas
#' \code{IQA_partial} e \code{missing_params}.
#'
#' @section Valores invalidos:
#' Concentracoes negativas, pH fora de \[0, 14\], temperatura fora de \[-2, 50\] C e
#' valores nao finitos sao convertidos em \code{NA} com aviso (e nao truncados
#' para zero).
#'
#' @param df Data frame ou tibble com as colunas de monitoramento.
#'   Nomes canonicos reconhecidos: \code{od}, \code{solidos_totais} (ou \code{residuo_total}),
#'   \code{ph}, \code{dbo}, \code{coliformes_termotolerantes} (ou \code{coliformes}),
#'   \code{nt_total}, \code{p_total}, \code{turbidez}, \code{temperatura}.
#' @param method Metodo de calculo:
#'   \itemize{
#'     \item \code{"CETESB"} (padrao) - equacoes analiticas por partes com media
#'           geometrica ponderada.
#'     \item \code{"CETESB_equations"} - alias de \code{"CETESB"}.
#'     \item \code{"IGAM"} - \strong{bloqueado}: as equacoes dos qi do IGAM
#'           (SCQA, SEMAD 2005) nao foram obtidas de fonte primaria. Gera erro de
#'           classe \code{tikatuwq_method_unavailable}; ver \code{\link{iqa_methods}}.
#'     \item \code{"CETESB_legacy_approx"} - subindices por curvas interpoladas aproximadas (legado).
#'     \item \code{"NSF_approx"} - subindices por curvas aproximadas com media aritmetica (legado).
#'   }
#' @param classification Depreciado desde 0.11.0. Por padrao (\code{NULL}) a
#'   escala segue \code{method}. Valores aceitos por compatibilidade:
#'   \code{"cetesb"}, \code{"ana"} (mesmos cortes da CETESB), \code{"igam"} e
#'   \code{"legacy"}; emitem aviso. Use \code{\link{classify_iqa}}.
#' @param altitude_m Altitude em metros (escalar ou vetor com uma altitude por
#'   linha), entre -500 e 9000 m; usada na concentracao de saturacao de OD.
#' @param phosphorus_basis Base do fosforo informado: \code{"P"} (default, aplica
#'   o fator 3,066 para PO4) ou \code{"PO4"}. A curva CETESB e de fosfato (PO4);
#'   o fator 3,066 consta no Apendice D 2023, p. 5.
#' @param microbial_type Tipo microbiologico informado: \code{"thermotolerant_coliforms"}
#'   (default) ou \code{"e_coli"} (multiplica por 1,25 para equivaler a
#'   termotolerantes, conforme o Apendice D CETESB 2023, p. 5).
#' @param temperature_method Componente termico: \code{"cetesb_default"} (default,
#'   qi = 94 constante; a CETESB usa "o valor correspondente a melhor qualidade",
#'   com \eqn{q^w = 1,58}, compativel com qi = 94), \code{"delta"} (usa \code{delta_temperature} ou a coluna
#'   \code{delta_temperatura}; erro se nenhum for informado) ou \code{"reference"}
#'   (usa \code{temperatura - temperature_reference}; erro se faltar algum).
#'   Os modos \code{"delta"} e \code{"reference"} sao **experimentais e nao
#'   validados**: a curva de Delta T do pacote nao tem fonte rastreavel e diverge
#'   da curva q6 da CETESB. Eles emitem aviso e marcam
#'   \code{"experimental_temperature_curve"} em \code{attr(, "iqa_nonstandard")}.
#'   Delta T < -5 C resulta em \code{NA} (qi indefinido pela fonte).
#' @param temperature_reference Temperatura de referencia (escalar ou um valor por
#'   linha) quando \code{temperature_method = "reference"}.
#' @param delta_temperature Variacao de temperatura (escalar ou um valor por
#'   linha) quando \code{temperature_method = "delta"}. Tem precedencia sobre a
#'   coluna \code{delta_temperatura}.
#' @param censor_policy Politica para dados censurados: \code{"limit"} (default: <X -> X),
#'   \code{"half_limit"} (<X -> X/2), \code{"zero"}, \code{"na"}, ou \code{"preserve"}.
#' @param allow_partial Logico; se \code{FALSE} (padrao), exige os 9 componentes.
#'   Se \code{TRUE}, calcula estimativa parcial (veja a secao Dados ausentes).
#' @param details Logico; se \code{TRUE}, anexa no atributo \code{"components"} a
#'   tabela de \code{\link{iqa_components}} com os pesos **efetivos** usados em
#'   cada linha, de modo que \code{prod(qi^wi)} reproduz \code{IQA_unrounded}.
#' @param add_status Logico; se \code{TRUE} (padrao), adiciona a coluna \code{IQA_status}.
#' @param locale Idioma dos rotulos qualitativos: \code{"pt"} (padrao) ou \code{"en"}.
#' @param pesos Vetor nomeado de pesos customizados (opcional). Nomes aceitos sao
#'   os canonicos ou aliases dos 9 parametros; componentes omitidos mantem o peso
#'   CETESB e o conjunto e renormalizado para soma 1. Pesos devem ser finitos e
#'   nao negativos. Com pesos customizados o resultado deixa de ser o IQA CETESB
#'   (registrado em \code{attr(, "iqa_nonstandard")}).
#' @param ... Apenas argumentos legados: \code{na_rm} (depreciado, use
#'   \code{allow_partial}) e \code{scheme} (depreciado, use \code{classification}).
#'   Outros argumentos geram aviso e sao ignorados.
#'
#' @returns
#' O data frame de entrada com a coluna numerica \code{IQA} (0-100, arredondada a
#' 2 casas) e, quando \code{add_status = TRUE}, a coluna fator \code{IQA_status}.
#' A classe e atribuida sobre o valor nao arredondado (\code{IQA_unrounded} nos
#' detalhes); o arredondamento e apenas de apresentacao.
#' Atributos de proveniencia: \code{iqa_method}, \code{iqa_method_reference},
#' \code{iqa_classification}, \code{iqa_weights}, \code{iqa_custom_weights},
#' \code{iqa_censor_policy}, \code{iqa_phosphorus_basis}, \code{iqa_microbial_type},
#' \code{iqa_temperature_method}, \code{iqa_altitude_m}, \code{iqa_allow_partial},
#' \code{iqa_partial_rows}, \code{iqa_complete} e \code{iqa_nonstandard}.
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
#'
#' # Outra escala para o mesmo numero: use classify_iqa()
#' classify_iqa(out$IQA, classification = "igam")
#'
#' # Metodos disponiveis (IGAM esta bloqueado)
#' iqa_methods()
#'
#' # Auditoria: pesos efetivos e termos por componente
#' comps <- attr(iqa(d, details = TRUE), "components")
#' prod(unlist(comps[1, grep("^term_", names(comps))]))
iqa <- function(
  df,
  method = c("CETESB", "CETESB_equations", "IGAM", "CETESB_legacy_approx", "NSF_approx"),
  classification = NULL,
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
  # Cada motor define sua escala de classificacao; 'classification' em iqa()
  # fica apenas por compatibilidade (depreciado desde 0.11.0).
  native_class   <- if (method == "IGAM") "igam" else "cetesb"
  classification_given <- !is.null(classification)
  if (classification_given) {
    classification <- match.arg(classification, c("cetesb", "igam", "ana", "legacy"))
  }
  phosphorus_basis <- match.arg(phosphorus_basis)
  microbial_type <- match.arg(microbial_type)
  temperature_method <- match.arg(temperature_method)
  censor_policy  <- match.arg(censor_policy)
  locale         <- match.arg(locale)

  # Argumentos legados aceitos via `...`:
  #   na_rm  -> alias depreciado de allow_partial
  #   scheme -> alias depreciado de classification
  dots <- list(...)
  if (!is.null(dots$na_rm)) {
    warning("Argument 'na_rm' is deprecated in iqa(); use 'allow_partial' instead.", call. = FALSE)
    if (isTRUE(dots$na_rm)) allow_partial <- TRUE
  }
  if (!is.null(dots$scheme)) {
    scheme <- match.arg(dots$scheme, c("cetesb", "igam", "ana", "legacy"))
    if (classification_given && scheme != classification) {
      stop("Conflicting arguments: 'scheme' and 'classification' differ. Use 'classification' only.")
    }
    warning("Argument 'scheme' is deprecated in iqa(); the scale now follows 'method'.", call. = FALSE)
    classification <- scheme
    classification_given <- TRUE
  }
  cross_classification <- FALSE
  if (classification_given) {
    # "ana" usa os mesmos cortes da CETESB: nao e classificacao cruzada
    same_scale <- classification == native_class ||
      (native_class == "cetesb" && classification == "ana")
    cross_classification <- !same_scale
    if (cross_classification) {
      warning("Cross-classification: the IQA is computed with method '", method,
              "' but classified on the '", classification, "' scale. This is not an ",
              toupper(classification), " IQA. Argument 'classification' in iqa() is ",
              "deprecated; use classify_iqa(x, classification = ...) instead.", call. = FALSE)
    } else {
      warning("Argument 'classification' in iqa() is deprecated: the scale follows ",
              "'method'. Use classify_iqa() for other scales.", call. = FALSE)
    }
  } else {
    classification <- native_class
  }

  if (method == "IGAM") {
    stop(.iqa_method_unavailable("IGAM", .iqa_igam_spec$blocked_reason, df))
  }
  unrecognized <- setdiff(names(dots), c("na_rm", "scheme"))
  if (length(unrecognized) > 0L || (length(dots) > 0L && is.null(names(dots)))) {
    warning("Unrecognized argument(s) passed to iqa() were ignored: ",
            paste(if (length(unrecognized)) unrecognized else "<unnamed>", collapse = ", "),
            call. = FALSE)
  }

  if (nrow(df) == 0L) {
    out <- df
    out$IQA <- numeric(0)
    if (isTRUE(add_status)) out$IQA_status <- classify_iqa(numeric(0), classification = classification, locale = locale)
    out <- .iqa_set_metadata(
      out,
      method = if (method == "CETESB_equations") "CETESB" else method,
      classification = classification,
      censor_policy = censor_policy,
      phosphorus_basis = phosphorus_basis,
      microbial_type = microbial_type,
      temperature_method = temperature_method,
      altitude_m = altitude_m,
      weights = if (is.null(pesos)) .iqa_cetesb_weights else pesos,
      custom_weights = !is.null(pesos),
      allow_partial = allow_partial,
      partial_rows = integer(0),
      nonstandard = if (cross_classification) "cross_classification" else character(0)
    )
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
      delta_temperature = delta_temperature,
      censor_policy = censor_policy
    )

    sfx     <- .iqa_comp_suffix
    qi_cols <- paste0("qi_", sfx)
    wi_cols <- paste0("wi_", sfx)
    qi_mat  <- as.matrix(comps[, qi_cols])

    # Pesos nominais (oficiais CETESB ou customizados)
    w_nominal <- .iqa_cetesb_weights
    custom_weights <- !is.null(pesos)
    if (custom_weights) w_nominal <- .iqa_resolve_weights(pesos, w_nominal)

    # Agregacao unica: pesos efetivos por linha (renormalizados sobre os
    # parametros disponiveis quando allow_partial = TRUE; 0 para ausentes)
    n <- nrow(df)
    w_eff   <- matrix(NA_real_, nrow = n, ncol = 9, dimnames = list(NULL, wi_cols))
    IQA_raw <- rep(NA_real_, n)
    partial <- rep(FALSE, n)
    missing_params <- rep(NA_character_, n)
    for (i in seq_len(n)) {
      ok <- is.finite(qi_mat[i, ]) & w_nominal > 0
      partial[i] <- !all(is.finite(qi_mat[i, ]) | w_nominal == 0)
      if (partial[i]) {
        missing_params[i] <- paste(names(w_nominal)[!is.finite(qi_mat[i, ]) & w_nominal > 0], collapse = ";")
      }
      if (!any(ok) || (!allow_partial && partial[i])) {
        w_eff[i, ] <- w_nominal / sum(w_nominal)
        next
      }
      ww <- ifelse(ok, w_nominal / sum(w_nominal[ok]), 0)
      w_eff[i, ] <- ww
      IQA_raw[i] <- prod(qi_mat[i, ok] ^ ww[ok])
    }

    # Sincroniza os detalhes com os pesos efetivamente usados
    for (k in seq_along(sfx)) {
      comps[[wi_cols[k]]] <- unname(w_eff[, k])
      comps[[paste0("term_", sfx[k])]] <- unname(ifelse(w_eff[, k] == 0, 1, comps[[qi_cols[k]]] ^ w_eff[, k]))
    }

    out <- df
    out$IQA <- round(pmin(100, pmax(0, IQA_raw)), 2)
    comps$IQA_unrounded  <- IQA_raw
    comps$IQA            <- out$IQA
    comps$IQA_partial    <- partial
    comps$missing_params <- missing_params

    # Classificacao sobre o valor NAO arredondado: o arredondamento a 2 casas e
    # so de apresentacao (ex.: 79.004 e Otima, embora seja exibido como 79.00).
    if (isTRUE(add_status)) {
      out$IQA_status <- classify_iqa(pmin(100, pmax(0, IQA_raw)), classification = classification, locale = locale)
    }

    if (isTRUE(details)) {
      attr(out, "components") <- comps
    }

    nonstandard <- if (cross_classification) "cross_classification" else character(0)
    if (custom_weights) nonstandard <- c(nonstandard, "custom_weights")
    if (any(partial & !is.na(IQA_raw))) nonstandard <- c(nonstandard, "partial_rows")
    if (temperature_method != "cetesb_default") nonstandard <- c(nonstandard, "experimental_temperature_curve")
    out <- .iqa_set_metadata(
      out,
      method = "CETESB",
      classification = classification,
      censor_policy = censor_policy,
      phosphorus_basis = phosphorus_basis,
      microbial_type = microbial_type,
      temperature_method = temperature_method,
      altitude_m = altitude_m,
      weights = w_nominal,
      custom_weights = custom_weights,
      allow_partial = allow_partial,
      partial_rows = which(partial & !is.na(IQA_raw)),
      nonstandard = nonstandard
    )

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

  custom_legacy <- !is.null(pesos)
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
    out$IQA_status <- classify_iqa(pmin(100, pmax(0, IQA_val)), classification = classification, locale = locale)
  }

  out <- .iqa_set_metadata(
    out,
    method = method,
    classification = classification,
    censor_policy = censor_policy,
    phosphorus_basis = NA_character_,
    microbial_type = NA_character_,
    temperature_method = NA_character_,
    altitude_m = NA_real_,
    weights = w_full,
    custom_weights = custom_legacy,
    allow_partial = allow_partial,
    partial_rows = which(!is.na(IQA_val) & rowSums(!is.finite(as.matrix(qi_df))) > 0),
    nonstandard = c("legacy_approximate_curves", if (custom_legacy) "custom_weights",
                    if (cross_classification) "cross_classification")
  )
  out
}

#' Resolve e valida pesos customizados do IQA
#' @param pesos Vetor numerico nomeado (nomes canonicos ou aliases).
#' @param base Pesos nominais de partida (9 componentes, nomes canonicos).
#' @return Vetor de 9 pesos (nomes canonicos); componentes nao informados
#'   mantem o peso de \code{base}.
#' @keywords internal
#' @noRd
.iqa_resolve_weights <- function(pesos, base = .iqa_cetesb_weights) {
  if (!is.numeric(pesos) || any(!is.finite(pesos)) || any(pesos < 0)) {
    stop("Custom weights ('pesos') must be non-negative, finite numeric values.")
  }
  if (is.null(names(pesos)) || any(names(pesos) == "")) {
    stop("Custom weights ('pesos') must be a named numeric vector (e.g. c(od = 0.2, dbo = 0.1)).")
  }
  canon <- unname(.alias_to_canonical[names(pesos)])
  canon[is.na(canon) & names(pesos) %in% names(base)] <- names(pesos)[is.na(canon) & names(pesos) %in% names(base)]
  unknown <- names(pesos)[is.na(canon) | !canon %in% names(base)]
  if (length(unknown)) {
    stop("Unknown parameter name(s) in 'pesos': ", paste(unknown, collapse = ", "),
         ". Valid names: ", paste(names(base), collapse = ", "), ".")
  }
  if (anyDuplicated(canon)) {
    stop("Duplicated parameter(s) in 'pesos' after alias resolution: ",
         paste(unique(canon[duplicated(canon)]), collapse = ", "), ".")
  }
  w <- base
  w[canon] <- unname(pesos)
  if (sum(w) <= 0) stop("Sum of custom weights ('pesos') must be greater than zero.")
  w
}

#' Anexa metadados de proveniencia ao resultado de iqa()
#' @keywords internal
#' @noRd
.iqa_set_metadata <- function(out, method, classification, censor_policy,
                              phosphorus_basis, microbial_type, temperature_method,
                              altitude_m, weights, custom_weights, allow_partial,
                              partial_rows, nonstandard) {
  attr(out, "iqa_method")             <- method
  attr(out, "iqa_calculation_method") <- method
  attr(out, "iqa_method_version")     <- paste0("tikatuwq ", as.character(utils::packageVersion("tikatuwq")))
  attr(out, "iqa_method_reference")   <- if (method == "CETESB") {
    "CETESB (2023), Apendice D - Indices de Qualidade das Aguas, secao 2 (pp. 3-5); qi por equacoes analiticas que aproximam as curvas da Figura 1 (ver inst/audit/IQA_AUDITORIA.md)"
  } else {
    "Curvas aproximadas por interpolacao (rota legada; nao corresponde a metodologia oficial)"
  }
  attr(out, "iqa_classification")     <- classification
  attr(out, "iqa_censor_policy")      <- censor_policy
  attr(out, "iqa_phosphorus_basis")   <- phosphorus_basis
  attr(out, "iqa_microbial_type")     <- microbial_type
  attr(out, "iqa_temperature_method") <- temperature_method
  attr(out, "iqa_altitude_m")         <- altitude_m
  attr(out, "iqa_weights")            <- weights
  attr(out, "iqa_custom_weights")     <- custom_weights
  attr(out, "iqa_allow_partial")      <- allow_partial
  attr(out, "iqa_partial_rows")       <- partial_rows
  attr(out, "iqa_complete")           <- length(partial_rows) == 0L
  attr(out, "iqa_nonstandard")        <- nonstandard
  out
}

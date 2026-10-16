# R/iqa_igam.R
# Nucleo do metodo IGAM (Minas Gerais), independente do metodo CETESB.
# (ASCII-only no codigo)
#
# ESTADO: disponivel em iqa(df, method = "IGAM") (liberado pelo mantenedor em
# 2026-10-16). Reproducao computacional documentada, NAO homologada pelo IGAM.
# Motor: .iqa_igam_compute(); rota de iqa(): .iqa_igam_route().
# Nota metodologica: inst/methodology/IGAM_VALIDATION.md.
#
# Fonte das equacoes: SEMAD/UCEMG/PNMA II (2005), "Sistema de Calculo da
# Qualidade da Agua (SCQA) - Estabelecimento das Equacoes do IQA", Relatorio 1,
# copia hospedada pelo IGAM (aguas.igam.mg.gov.br/aguas/downloads/
# SCQA_final.pdf; Internet Archive 2007, SHA-1 conferido). Equacoes, fontes
# e validacao: inst/methodology/IGAM_VALIDATION.md. Os relatorios de pesquisa
# com registros por amostra ficam no repositorio de desenvolvimento (audit/ e
# inst/audit/, excluidos do pacote distribuido).
#
# Turbidez: o SCQA impresso traz "-15 cos(...)", que nao reproduz a Figura 2.13
# do proprio documento nem a curva do portal IGAM. O motor usa k = 1.5
# ("-1,5 cos(...)"), coeficiente registrado na literatura (Bucci, UFJF 2011,
# p. 51, que usou o programa de calculo cedido pelo IGAM; Nunes, UFV 2008,
# Quadro 10) e sustentado pela reproducao de 49/49 IQAs publicados. Nao ha
# confirmacao formal do IGAM.
#
# Regras do Projeto Aguas de Minas (portal IGAM): qs de temperatura = 92; sem
# OD e/ou microbiologico o indice nao e calculado; peso de ausente repartido
# igualmente. Nitrato: curva em mg/L NO3 (SCQA Tabela 2.1; portal, Tabela 1).
# Ver inst/methodology/IGAM_VALIDATION.md, secao 6.

# Especificacao do metodo IGAM. Status: a = oficial literal; b = secundaria;
# c = hipotese documentada na literatura; d = nao verificado/ausente.
.iqa_igam_spec <- list(
  method = "IGAM",
  version = "SCQA (SEMAD/UCEMG/PNMA II, junho 2005), Relatorio 1; Projeto Aguas de Minas; turbidez k = 1.5",
  implemented = TRUE,
  documented = TRUE,
  # reference_validated = reproduz valores de IQA publicados pela instituicao
  # (49/49 IGAM, Pampulha 2017-2018). NAO significa homologacao pelo IGAM nem
  # validacao em outras bacias.
  reference_validated = TRUE,
  available = TRUE,
  # Usado so se available voltar a FALSE
  blocked_reason = "the IGAM engine has been disabled in this build.",
  note = paste(
    "IQA-IGAM: documented computational reproduction of the IGAM method (SCQA",
    "2005 equations; Projeto Aguas de Minas rules). Not endorsed or certified",
    "by IGAM. Turbidity uses k = 1.5 (Bucci 2011; Nunes 2008) instead of the",
    "k = 15 printed in some SCQA copies. Reproduces 49/49 published IGAM IQAs",
    "(Pampulha sub-basin, 2017-2018). See system.file('methodology',",
    "'IGAM_VALIDATION.md', package = 'tikatuwq')."
  ),
  # Parametros: SCQA 2005, Tabela 2.1 (a); E. coli: relatorio IGAM 2017, p. 9 (a)
  parameters = c(
    od = "oxigenio dissolvido (mg/L) -> % saturacao; Cs do SCQA usa temperatura, cloreto e altitude",
    microbiologico = "E. coli ou coliformes termotolerantes (NMP/100 mL), sem fator de conversao",
    ph = "pH",
    dbo = "DBO5,20 (mg/L)",
    nitrato = "nitrato; curva em mg/L NO3 (entrada em mg/L N e convertida x 62.004/14.007)",
    fosfato_total = "fosfato total (mg/L PO4); fosforo total (mg/L P) convertido x 94.9714/30.9738",
    temperatura = "variacao de temperatura: qs constante = 92 (Projeto Aguas de Minas)",
    turbidez = "turbidez (UNT); k = 1.5 (hipotese de correcao)",
    solidos_totais = "solidos totais (mg/L)"
  ),
  # Pesos: SCQA 2005, Tabela 2.1 (a)
  weights = c(
    od = 0.17, microbiologico = 0.15, ph = 0.12, dbo = 0.10, nitrato = 0.10,
    fosfato_total = 0.10, temperatura = 0.10, turbidez = 0.08, solidos_totais = 0.08
  ),
  weights_status = "a",
  # Portal IGAM: sem OD e/ou coliformes/E. coli o indicador nao e calculado (a)
  mandatory = c("od", "microbiologico"),
  # Portal IGAM: qs constante = 92 no Projeto Aguas de Minas (a). O SCQA 2005
  # (p. 12) da 93 na curva em dT = 0; nao e usado (outro contexto).
  temperature_qs = 92,
  temperature_qs_scqa_2005 = 93,
  turbidity_k = 1.5,
  turbidity_k_printed = 15,
  # SCQA 2005, Tabela 2.2 (a); igual ao relatorio IGAM 2017, p. 9
  classification = "igam",
  qi_equations = "SCQA 2005 (R/iqa_igam.R), turbidez com k = 1.5",
  qi_status = "8 equacoes SCQA (a); turbidez k = 1.5 (c); reproduz 49/49 IQAs IGAM publicados (Pampulha 2017-2018)"
)

# Fatores estequiometricos (massas molares IUPAC)
.igam_n_to_no3 <- 62.004 / 14.007
.igam_p_to_po4 <- 94.9714 / 30.9738

# ------------------------------------------------------------------------------
# Subindices qi do SCQA 2005 (pagina do documento entre parenteses).
# Funcoes puras e vetorizadas; NA entra, NA sai. Sem truncamento em 100: o SCQA
# nao o define (maximos: OD 100.02, nitrato 100.17, turbidez 100.003).
# ------------------------------------------------------------------------------

# Concentracao de saturacao de OD (mg/L) (p. 3): temperatura (C), cloreto
# (mg/L) e altitude (m)
.igam_cs <- function(temp, chloride, altitude_m) {
  (14.2 * exp(-0.0212 * temp) - 0.0016 * chloride * exp(-0.0264 * temp)) *
    (0.994 - 0.0001042 * altitude_m)
}

# OD (% saturacao) (p. 4). y3 lido como pi/85 x (OD% - 15): o documento
# imprime um traco, mas so a multiplicacao reproduz as Figuras 2.1/2.2 do SCQA
# e a curva do portal IGAM (q = 3.1 em 0%).
.qi_igam_od <- function(psat) {
  out <- rep(NA_real_, length(psat))
  i <- which(psat <= 100)
  if (length(i)) {
    x <- psat[i]
    y1 <- 0.01396 * x + 0.0873
    y2 <- pi / 56 * (x - 27)
    y3 <- pi / 85 * (x - 15)
    y4 <- (x - 65) / 10
    y5 <- (65 - x) / 10
    out[i] <- 100 * sin(y1)^2 - ((2.5 * sin(y2) - 0.018 * x + 6.86) * sin(y3)) +
      12 / (exp(y4) + exp(y5))
  }
  i <- which(psat > 100 & psat <= 140)
  out[i] <- -0.00777142857142832 * psat[i]^2 + 1.27854285714278 * psat[i] + 49.8817148572
  out[which(psat > 140)] <- 47
  out
}

# Coliformes (NMP/100 mL) (p. 5); log10. Abaixo de 1 NMP/100 mL o dominio nao
# e definido pelo SCQA: NA (sem valor inventado).
.qi_igam_coliformes <- function(cf) {
  out <- rep(NA_real_, length(cf))
  i <- which(cf >= 1 & cf <= 1e5)
  L <- log10(cf[i])
  out[i] <- 98.24034 - 34.7145 * L + 2.614267 * L^2 + 0.107821 * L^3
  out[which(cf > 1e5)] <- 3
  out
}

# pH (p. 6). Em pH = 12 vale o trecho "<= 12" (o texto sobrepoe "<= 12" e ">= 12").
.qi_igam_ph <- function(ph) {
  out <- rep(NA_real_, length(ph))
  out[which(ph <= 2)] <- 2
  i <- which(ph > 2 & ph <= 6.9)
  p <- ph[i]
  out[i] <- -37.1085 + 41.91277 * p - 15.7043 * p^2 + 2.417486 * p^3 - 0.091252 * p^4
  i <- which(ph > 6.9 & ph <= 7.1)
  p <- ph[i]
  out[i] <- -4.69365 - 21.4593 * p - 68.4561 * p^2 + 21.638886 * p^3 - 1.59165 * p^4
  i <- which(ph > 7.1 & ph <= 12)
  p <- ph[i]
  out[i] <- -7698.19 + 3262.031 * p - 499.494 * p^2 + 33.1551 * p^3 - 0.810613 * p^4
  out[which(ph > 12)] <- 3
  out
}

# DBO (mg/L) (p. 7)
.qi_igam_dbo <- function(dbo) {
  ifelse(dbo <= 30,
         100.9571 - 10.7121 * dbo + 0.49544 * dbo^2 - 0.011167 * dbo^3 + 0.0001 * dbo^4,
         2)
}

# Nitrato em mg/L NO3 (p. 8). ln no trecho 10-60 (SCQA: "ln").
.qi_igam_nitrato <- function(no3) {
  ifelse(no3 <= 10, -5.1 * no3 + 100.17,
         ifelse(no3 <= 60, -22.853 * log(no3) + 101.18,
                ifelse(no3 <= 90, 1e10 * no3^-5.1161, 1)))
}

# Fosfato total em mg/L PO4 (p. 9)
.qi_igam_fosfato <- function(po4) {
  ifelse(po4 <= 10, 79.7 * (po4 + 0.821)^-1.15, 5)
}

# Turbidez (UNT) (p. 10). k = 1.5: hipotese de correcao do "15" impresso
# (cosseno em radianos). Tu > 100: q = 5 (SCQA p. 10; salto em Tu = 100 e da
# fonte e nao e suavizado). k = 15 so para auditoria.
.qi_igam_turbidez <- function(tu, k = .iqa_igam_spec$turbidity_k) {
  ifelse(tu <= 100,
         90.37 * exp(-0.0169 * tu) - k * cos(0.0571 * (tu - 30)) +
           10.22 * exp(-0.231 * tu) - 0.8,
         5)
}

# Solidos totais (mg/L) (p. 11)
.qi_igam_solidos <- function(st) {
  ifelse(st <= 500,
         133.17 * exp(-0.0027 * st) - 53.17 * exp(-0.0141 * st) +
           (-6.2 * exp(-0.00462 * st)) * sin(0.0146 * st),
         30)
}

#' Pesos efetivos do IGAM com parametros ausentes
#'
#' Regra do portal IGAM: "o peso (do parametro ausente) e repartido igualmente
#' entre os demais parametros". Para mais de um ausente, a soma dos pesos
#' ausentes e repartida igualmente (interpretacao; a fonte so descreve o caso
#' geral). Sem OD ou microbiologico o indice nao e calculado.
#'
#' @param available Vetor logico nomeado (nomes de .iqa_igam_spec$weights).
#' @return Vetor de pesos efetivos (0 para ausentes) ou NULL se um parametro
#'   obrigatorio faltar.
#' @keywords internal
#' @noRd
.igam_effective_weights <- function(available, spec = .iqa_igam_spec) {
  w <- spec$weights
  available <- available[names(w)]
  available[is.na(available)] <- FALSE
  if (!all(available[spec$mandatory])) return(NULL)
  if (all(available)) return(w)
  missing_w <- sum(w[!available])
  out <- ifelse(available, w + missing_w / sum(available), 0)
  names(out) <- names(w)
  out
}

#' Verifica as colunas exigidas pelo metodo IGAM
#'
#' Nao usa substitutos automaticos: nitrogenio total nao substitui nitrato,
#' ortofosfato nao substitui fosfato total e fosforo total (P) so e aceito com
#' \code{phosphorus_input = "P"} explicito.
#' @return Lista com colunas resolvidas e mensagens de problemas.
#' @keywords internal
#' @noRd
.igam_check_inputs <- function(df, phosphorus_input = "PO4", microbial = "auto") {
  ec_col <- .resolve_col(df, "e_coli")
  ct_col <- .resolve_col(df, "coliformes_termotolerantes")
  micro_type <- switch(microbial,
    e_coli = "e_coli",
    thermotolerant_coliforms = "thermotolerant_coliforms",
    if (!is.null(ec_col)) "e_coli" else "thermotolerant_coliforms")
  micro_col <- if (micro_type == "e_coli") ec_col else ct_col
  cols <- list(
    od = .resolve_col(df, "od"),
    temperatura_agua = .resolve_col(df, "temperatura"),
    cloreto = .resolve_col(df, "cloreto"),
    microbiologico = micro_col,
    ph = .resolve_col(df, "ph"),
    dbo = .resolve_col(df, "dbo"),
    nitrato = .resolve_col(df, "n_nitrato"),
    fosfato_total = if (identical(phosphorus_input, "P")) .resolve_col(df, "p_total") else .resolve_col(df, "fosfato_total"),
    turbidez = .resolve_col(df, "turbidez"),
    solidos_totais = .resolve_col(df, "solidos_totais")
  )
  problems <- character(0)
  if (is.null(cols$nitrato) && !is.null(.resolve_col(df, "nt_total"))) {
    problems <- c(problems, "IGAM requires nitrate ('nitrato'); total nitrogen ('nt_total') is a different analyte and is not used as a substitute.")
  }
  if (is.null(cols$fosfato_total)) {
    problems <- c(problems, if (!identical(phosphorus_input, "P") && !is.null(.resolve_col(df, "p_total"))) {
      "IGAM uses total phosphate as PO4 ('fosfato_total'); 'p_total' (as P) is only converted with phosphorus_input = 'P'."
    } else if (!is.null(.resolve_col(df, "p_ortofosfato"))) {
      "IGAM uses total phosphate ('fosfato_total'); orthophosphate is a different analyte."
    } else "IGAM requires total phosphate ('fosfato_total', as PO4) or total phosphorus ('p_total', as P).")
  }
  if (is.null(cols$solidos_totais) && !is.null(.resolve_col(df, "tds"))) {
    problems <- c(problems, "IGAM requires total solids; TDS ('tds') is not interchangeable.")
  }
  if (is.null(cols$od)) problems <- c(problems, "IGAM requires dissolved oxygen ('od'): the index is not computed without it.")
  if (is.null(cols$microbiologico)) problems <- c(problems, "IGAM requires thermotolerant coliforms or E. coli: the index is not computed without them.")
  if (!is.null(cols$od) && is.null(cols$temperatura_agua)) problems <- c(problems, "IGAM dissolved-oxygen saturation (SCQA) requires water temperature ('temperatura').")
  if (!is.null(cols$od) && is.null(cols$cloreto)) problems <- c(problems, "IGAM dissolved-oxygen saturation (SCQA) requires chloride ('cloreto'); no default is assumed.")
  list(cols = cols, problems = problems, micro_type = micro_type,
       micro_ambiguous = microbial == "auto" && !is.null(ec_col) && !is.null(ct_col))
}

#' Motor IGAM experimental (interno)
#'
#' Calcula o IQA pelo SCQA 2005 com turbidez k = 1.5 e as regras do Projeto
#' Aguas de Minas. Nao exposto em \code{iqa()} enquanto
#' \code{.iqa_igam_spec$available} for \code{FALSE}.
#'
#' @param df Data frame com as observacoes.
#' @param nitrate_unit Unidade do nitrato informado: \code{"N"} (mg/L N-NO3,
#'   convertido x 62.004/14.007) ou \code{"NO3"} (mg/L NO3). Obrigatorio se
#'   houver coluna de nitrato.
#' @param phosphorus_input \code{"PO4"} (coluna \code{fosfato_total}, mg/L
#'   PO4) ou \code{"P"} (coluna \code{p_total}, mg/L P, convertido x
#'   94.9714/30.9738).
#' @param altitude_m Altitude (m), escalar ou um valor por linha.
#' @param censor_policy Politica para valores censurados ("<x", ">x", "ND"),
#'   repassada a \code{.parse_nd_ld()}; registrada no resultado.
#' @param turbidity_k Coeficiente do cosseno da turbidez (1.5; 15 so para
#'   auditoria da expressao impressa).
#' @param microbial \code{"auto"} (usa a unica coluna microbiologica presente;
#'   erro se houver E. coli e termotolerantes ao mesmo tempo), \code{"e_coli"}
#'   ou \code{"thermotolerant_coliforms"}. Nenhum fator de conversao.
#' @return Data frame com \code{IQA_raw}, \code{IQA} (1 casa, como publicado
#'   pelo IGAM), \code{IQA_status}, qi, pesos efetivos e motivos; atributos com
#'   a configuracao e a contagem de censurados.
#' @keywords internal
#' @noRd
.iqa_igam_compute <- function(df,
                              nitrate_unit = NULL,
                              phosphorus_input = c("PO4", "P"),
                              altitude_m = 0,
                              censor_policy = c("limit", "half_limit", "zero", "na"),
                              turbidity_k = .iqa_igam_spec$turbidity_k,
                              microbial = c("auto", "e_coli", "thermotolerant_coliforms")) {
  if (!is.data.frame(df)) stop("'df' must be a data frame.")
  phosphorus_input <- match.arg(phosphorus_input)
  censor_policy <- match.arg(censor_policy)
  microbial <- match.arg(microbial)
  n <- nrow(df)
  if (!is.numeric(altitude_m) || any(!is.finite(altitude_m)) ||
      !(length(altitude_m) == 1L || length(altitude_m) == n)) {
    stop("altitude_m must be a finite numeric value or one value per row.")
  }
  if (any(altitude_m < -500 | altitude_m > 9000)) stop("altitude_m must be between -500 and 9000 m.")
  alt <- rep_len(as.numeric(altitude_m), n)

  chk <- .igam_check_inputs(df, phosphorus_input, microbial)
  cols <- chk$cols
  if (chk$micro_ambiguous) {
    stop("Both E. coli and thermotolerant coliform columns are present: set ",
         "microbial_type ('e_coli' or 'thermotolerant_coliforms') to choose one. ",
         "IGAM uses either without a conversion factor.", call. = FALSE)
  }
  if (!is.null(cols$nitrato)) {
    if (is.null(nitrate_unit)) {
      stop("nitrate_unit must be given explicitly ('N' for mg/L N-NO3 or 'NO3' for mg/L NO3): ",
           "the IGAM curve is in mg/L NO3 and no unit is assumed.")
    }
    nitrate_unit <- match.arg(nitrate_unit, c("N", "NO3"))
  }

  censored <- list()
  get <- function(key, name, lower = 0, upper = Inf) {
    col <- cols[[key]]
    if (is.null(col)) return(rep(NA_real_, n))
    raw <- df[[col]]
    censored[[key]] <<- if (is.numeric(raw)) rep(FALSE, n) else
      grepl("^\\s*[<>]|^\\s*ND\\s*$", as.character(raw), ignore.case = TRUE)
    v <- suppressWarnings(as.numeric(.parse_nd_ld(raw, censor_policy)))
    .iqa_flag_invalid(v, name, lower = lower, upper = upper)
  }
  v_od   <- get("od", "od")
  v_temp <- get("temperatura_agua", "temperatura", lower = -2, upper = 50)
  v_cl   <- get("cloreto", "cloreto")
  v_mic  <- get("microbiologico", "microbiologico")
  v_ph   <- get("ph", "ph", upper = 14)
  v_dbo  <- get("dbo", "dbo")
  v_no3  <- get("nitrato", "nitrato")
  v_po4  <- get("fosfato_total", "fosfato_total")
  v_tu   <- get("turbidez", "turbidez")
  v_st   <- get("solidos_totais", "solidos_totais")

  if (!is.null(cols$nitrato) && nitrate_unit == "N") v_no3 <- v_no3 * .igam_n_to_no3
  if (phosphorus_input == "P") v_po4 <- v_po4 * .igam_p_to_po4

  cs <- .igam_cs(v_temp, v_cl, alt)
  psat <- 100 * v_od / cs
  psat[!is.finite(psat) | cs <= 0] <- NA_real_

  q <- cbind(
    od = .qi_igam_od(psat),
    microbiologico = .qi_igam_coliformes(v_mic),
    ph = .qi_igam_ph(v_ph),
    dbo = .qi_igam_dbo(v_dbo),
    nitrato = .qi_igam_nitrato(v_no3),
    fosfato_total = .qi_igam_fosfato(v_po4),
    temperatura = rep(.iqa_igam_spec$temperature_qs, n),
    turbidez = .qi_igam_turbidez(v_tu, k = turbidity_k),
    solidos_totais = .qi_igam_solidos(v_st)
  )
  wn <- names(.iqa_igam_spec$weights)
  W <- matrix(0, n, length(wn), dimnames = list(NULL, wn))
  IQA_raw <- rep(NA_real_, n)
  reason <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    ok <- is.finite(q[i, wn])
    names(ok) <- wn
    w <- .igam_effective_weights(ok)
    if (is.null(w)) {
      why <- character(0)
      if (!ok[["od"]]) {
        why <- c(why, if (is.na(v_od[i])) "od ausente/invalido"
                 else if (is.na(v_temp[i])) "temperatura ausente (saturacao de OD)"
                 else if (is.na(v_cl[i])) "cloreto ausente (saturacao de OD)"
                 else "saturacao de OD nao calculavel")
      }
      if (!ok[["microbiologico"]]) {
        why <- c(why, if (!is.na(v_mic[i]) && v_mic[i] < 1) "microbiologico < 1 NMP/100 mL (fora do dominio SCQA)"
                 else "microbiologico ausente/invalido")
      }
      reason[i] <- paste(why, collapse = "; ")
      next
    }
    W[i, ] <- w
    used <- w > 0
    IQA_raw[i] <- prod(q[i, wn][used]^w[used])
    if (any(!ok)) reason[i] <- paste0("pesos redistribuidos; ausentes: ", paste(wn[!ok], collapse = ", "))
  }
  colnames(q) <- paste0("qi_", colnames(q))
  colnames(W) <- paste0("w_", colnames(W))
  IQA <- round(IQA_raw, 1)
  out <- data.frame(
    IQA_raw = IQA_raw,
    IQA = IQA,
    IQA_status = classify_iqa(IQA, classification = "igam"),
    od_saturacao_pct = psat,
    nitrato_mgL_NO3 = v_no3,
    fosfato_total_mgL_PO4 = v_po4,
    q, W,
    observacao = reason,
    stringsAsFactors = FALSE
  )
  attr(out, "iqa_calculation_method") <- "IGAM"
  attr(out, "iqa_method_version") <- .iqa_igam_spec$version
  attr(out, "iqa_classification") <- "igam"
  attr(out, "igam_config") <- list(
    turbidity_k = turbidity_k, temperature_qs = .iqa_igam_spec$temperature_qs,
    nitrate_unit = nitrate_unit, phosphorus_input = phosphorus_input,
    microbial_column = cols$microbiologico, microbial_type = chk$micro_type,
    censor_policy = censor_policy,
    status = "documented computational reproduction; not endorsed by IGAM"
  )
  attr(out, "igam_censored") <- as.data.frame(censored)
  out
}

#' Rota IGAM de iqa(): valida argumentos, resolve fosforo/microbiologico sem
#' conversao implicita, chama o motor e monta a saida no formato de iqa().
#' @param given Lista logica: quais argumentos de iqa() foram informados.
#' @keywords internal
#' @noRd
.iqa_igam_route <- function(df, altitude_m, phosphorus_basis, microbial_type,
                            temperature_method, temperature_reference,
                            delta_temperature, censor_policy, allow_partial,
                            details, add_status, locale, pesos, nitrate_unit,
                            given) {
  # Opcoes do motor CETESB que nao existem no metodo IGAM: erro, nao ignorar
  if (!is.null(pesos)) stop("Custom weights ('pesos') are not available for method = 'IGAM'.", call. = FALSE)
  if (isTRUE(allow_partial)) {
    stop("allow_partial is not used by method = 'IGAM': the IGAM rule applies ",
         "(no index without dissolved oxygen or the microbial parameter; other ",
         "missing weights are shared equally).", call. = FALSE)
  }
  if (temperature_method != "cetesb_default" || !is.null(temperature_reference) || !is.null(delta_temperature)) {
    stop("method = 'IGAM' uses the constant temperature sub-index qs = 92 ",
         "(Projeto Aguas de Minas); temperature_method, temperature_reference ",
         "and delta_temperature do not apply.", call. = FALSE)
  }
  if (censor_policy == "preserve") {
    stop("censor_policy = 'preserve' is not available for method = 'IGAM'.", call. = FALSE)
  }

  # Fosforo: 'fosfato_total' (PO4) e usado direto; 'p_total' exige base declarada
  has_po4 <- !is.null(.resolve_col(df, "fosfato_total"))
  has_p <- !is.null(.resolve_col(df, "p_total"))
  phosphorus_input <- "PO4"
  if (has_po4) {
    if (given$phosphorus_basis && phosphorus_basis == "P") {
      stop("Column 'fosfato_total' is total phosphate as PO4; phosphorus_basis = 'P' ",
           "contradicts it. Remove phosphorus_basis or supply 'p_total' instead.", call. = FALSE)
    }
  } else if (has_p) {
    if (!given$phosphorus_basis) {
      stop("method = 'IGAM' uses total phosphate as PO4. Declare the basis of ",
           "'p_total': phosphorus_basis = 'P' (mg/L P, converted x 94.9714/30.9738) ",
           "or 'PO4' (already mg/L PO4).", call. = FALSE)
    }
    if (phosphorus_basis == "P") {
      phosphorus_input <- "P"
    } else {
      df[["fosfato_total"]] <- df[[.resolve_col(df, "p_total")]]
    }
  }
  microbial <- if (given$microbial_type) microbial_type else "auto"

  res <- .iqa_igam_compute(df, nitrate_unit = nitrate_unit, phosphorus_input = phosphorus_input,
                           altitude_m = altitude_m, censor_policy = censor_policy,
                           microbial = microbial)
  cfg <- attr(res, "igam_config")
  out <- df
  if (!has_po4 && has_p && phosphorus_basis == "PO4") out[["fosfato_total"]] <- NULL
  out$IQA <- res$IQA
  if (isTRUE(add_status)) out$IQA_status <- classify_iqa(res$IQA, classification = "igam", locale = locale)
  redistributed <- which(!is.na(res$IQA_raw) & !is.na(res$observacao))
  if (isTRUE(details)) {
    comps <- res
    comps$IQA_unrounded <- res$IQA_raw
    comps$missing_params <- ifelse(is.na(res$IQA_raw), res$observacao, NA_character_)
    attr(out, "components") <- comps
  }
  out <- .iqa_set_metadata(
    out, method = "IGAM", classification = "igam", censor_policy = censor_policy,
    phosphorus_basis = if (phosphorus_input == "P") "P" else "PO4",
    microbial_type = cfg$microbial_type, temperature_method = "igam_constant_92",
    altitude_m = altitude_m, weights = .iqa_igam_spec$weights, custom_weights = FALSE,
    allow_partial = FALSE, partial_rows = redistributed,
    nonstandard = character(0)
  )
  attr(out, "iqa_method_version") <- paste0(.iqa_igam_spec$version, "; tikatuwq ",
                                            as.character(utils::packageVersion("tikatuwq")))
  attr(out, "iqa_nitrate_unit") <- cfg$nitrate_unit
  attr(out, "iqa_igam_config") <- cfg
  attr(out, "iqa_igam_censored") <- attr(res, "igam_censored")
  attr(out, "iqa_note") <- .iqa_igam_spec$note
  out
}

#' Erro de metodo indisponivel (classe propria, para compare_iqa())
#' @keywords internal
#' @noRd
.iqa_method_unavailable <- function(method, reason, df = NULL) {
  msg <- paste0("method = '", method, "' is not available: ", reason)
  if (!is.null(df) && method == "IGAM") {
    chk <- .igam_check_inputs(df)
    if (length(chk$problems)) msg <- paste0(msg, " Input issues also found: ", paste(chk$problems, collapse = " "))
  }
  msg <- paste0(msg, " See iqa_methods().")
  structure(class = c("tikatuwq_method_unavailable", "error", "condition"),
            list(message = msg, call = NULL, method = method))
}

#' Metodos de calculo do IQA disponiveis no pacote
#'
#' @description
#' Lista os motores de calculo do IQA, sua situacao de validacao e a escala de
#' classificacao que cada um aplica por padrao. Use para verificar, antes de
#' calcular, se um metodo esta disponivel.
#'
#' @return Data frame com as colunas \code{method}, \code{implemented}
#'   (motor presente no pacote), \code{documented} (fontes registradas),
#'   \code{reference_validated}, \code{available} (pode ser usado em
#'   \code{iqa()}), \code{classification}, \code{limitation} e
#'   \code{reference}.
#'
#'   \strong{Semantica de \code{reference_validated}:} \code{TRUE} significa
#'   que o motor reproduz valores de IQA \emph{publicados} pela fonte de
#'   referencia (exemplo resolvido, no caso CETESB; IQAs por amostra publicados
#'   pelo IGAM, no caso IGAM), com o escopo descrito em \code{limitation}.
#'   \strong{Nao} significa homologacao, certificacao ou endosso da instituicao,
#'   nem validacao em todas as bacias ou condicoes analiticas.
#'
#' @details
#' \itemize{
#'   \item \code{"CETESB"}: padrao. Equacoes que aproximam as curvas da CETESB
#'     (Apendice D 2023); ver \code{system.file("audit", "VALIDACAO_CETESB.md",
#'     package = "tikatuwq")}.
#'   \item \code{"IGAM"}: reproducao computacional documentada do IQA do IGAM
#'     (equacoes do SCQA, SEMAD 2005; regras do Projeto Aguas de Minas), com a
#'     turbidez em \code{k = 1,5} (Bucci 2011; Nunes 2008). Reproduz 49/49 IQAs
#'     publicados pelo IGAM na sub-bacia da Pampulha (2017-2018).
#'     \strong{Nao e homologada pelo IGAM.} Ver \code{system.file("methodology",
#'     "IGAM_VALIDATION.md", package = "tikatuwq")}.
#'   \item \code{"CETESB_legacy_approx"} e \code{"NSF_approx"}: rotas legadas
#'     por curvas interpoladas aproximadas.
#' }
#' A ANA nao tem metodo proprio no pacote: o portal da ANA usa os parametros e
#' pesos da CETESB, mas a equivalencia das curvas nao foi verificada (ver
#' \code{system.file("audit", "ANA_VS_CETESB.md", package = "tikatuwq")}).
#'
#' @examples
#' iqa_methods()
#' @export
iqa_methods <- function() {
  data.frame(
    method = c("CETESB", "IGAM", "CETESB_legacy_approx", "NSF_approx"),
    implemented = c(TRUE, .iqa_igam_spec$implemented, TRUE, TRUE),
    documented = c(TRUE, .iqa_igam_spec$documented, TRUE, TRUE),
    reference_validated = c(TRUE, .iqa_igam_spec$reference_validated, FALSE, FALSE),
    available = c(TRUE, .iqa_igam_spec$available, TRUE, TRUE),
    classification = c("cetesb", "igam", "cetesb", "cetesb"),
    limitation = c(
      "qi equations approximate the graphical CETESB curves (secondary sources); validated against a published worked example (von Sperling 2007 via IBEAS 2015); pH 9-10 and delta-T curve pending",
      "not endorsed by IGAM; turbidity k = 1.5 from the literature (SCQA copies print 15); reproduces 49/49 published IGAM IQAs, all from the Pampulha sub-basin (2017-2018), nitrate mostly censored; requires chloride and an explicit nitrate unit",
      "legacy; interpolated approximate curves",
      "legacy; interpolated approximate curves, arithmetic mean"
    ),
    reference = c("CETESB (2023) Apendice D; CPRH PNMA II; von Sperling (2007) via IBEAS (2015)",
                  "SEMAD/UCEMG/PNMA II (2005) SCQA Relatorio 1; IGAM portal; Bucci (2011, UFJF); Nunes (2008, UFV)",
                  "-", "-"),
    stringsAsFactors = FALSE
  )
}

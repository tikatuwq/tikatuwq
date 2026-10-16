# R/iqa_equations.R
# Equacoes analiticas que aproximam as curvas graficas do IQA CETESB e camada
# de auditoria de componentes.
#
# Proveniencia (ver inst/audit/IQA_AUDITORIA.md e MATRIZ_VALIDACAO_QI.csv):
# - CETESB (2023), Apendice D, Figura 1: so curvas graficas + valores fixos.
# - Equacoes por trechos: planilha CETESB transcrita em CPRH, PNMA II
#   "Indices e indicadores de qualidade da agua" (Pernambuco) e em
#   von Sperling (2007, p. 255), reproduzida por IBEAS (2015, VIII-041).
#   As duas transcricoes divergem em DBO 5-15, NT 10-60 (base do log),
#   pH 9-10 e OD > 140%; as escolhas abaixo seguem a curva oficial.
# (ASCII-only no codigo)

# ------------------------------------------------------------------------------
# Helpers individuais de Qi por parametro (funcoes puras e vetorizadas)
# ------------------------------------------------------------------------------

#' Qi do Oxigenio Dissolvido (CETESB oficial via % saturacao)
#' @param od Oxigenio dissolvido em mg/L.
#' @param temp Temperatura da agua em graus Celsius (obrigatoria para saturacao).
#' @param altitude_m Altitude em metros acima do nivel do mar (default 0).
#' @return Vetor numerico com Qi (0-100).
#' @keywords internal
#' @noRd
.qi_cetesb_od <- function(od, temp, altitude_m = 0) {
  if (is.null(od) || is.null(temp)) return(NULL)
  od   <- as.numeric(od)
  temp <- as.numeric(temp)
  altitude_m <- as.numeric(altitude_m)

  # Concentracao de saturacao Cs (mg/L) corrigida por temperatura e altitude.
  # Nao consta do Apendice D CETESB 2023 (que so diz que a %sat depende de
  # altitude e temperatura). Coeficientes conforme CPRH/PNMA II (planilha
  # CETESB) e von Sperling (2007, p. 255, via IBEAS 2015). Ate a v0.10.0 o
  # termo cubico era 0.00005898 (erro de transcricao; impacto < 0.02% em Cs).
  Cs <- (14.62 - 0.3898 * temp + 0.006969 * (temp^2) - 0.00005896 * (temp^3)) *
        ((1 - 0.0000228675 * altitude_m)^5.167)
  
  # Evita divisao por zero
  Cs <- pmax(Cs, 0.01)
  psat <- pmax(0, 100 * (od / Cs))

  out <- rep(NA_real_, length(psat))
  
  # 0 <= psat <= 50
  i <- which(psat <= 50)
  if (length(i)) {
    out[i] <- 3 + 0.34 * psat[i] + 0.008095 * (psat[i]^2) + 1.35252e-05 * (psat[i]^3)
  }
  # 50 < psat <= 85
  i <- which(psat > 50 & psat <= 85)
  if (length(i)) {
    out[i] <- 3 - 1.166 * psat[i] + 0.058 * (psat[i]^2) - 3.803435e-04 * (psat[i]^3)
  }
  # 85 < psat <= 100
  i <- which(psat > 85 & psat <= 100)
  if (length(i)) {
    out[i] <- 3 + 3.7745 * (psat[i]^0.704889)
  }
  # 100 < psat <= 140
  i <- which(psat > 100 & psat <= 140)
  if (length(i)) {
    out[i] <- 3 + 2.9 * psat[i] - 0.02496 * (psat[i]^2) + 5.60919e-05 * (psat[i]^3)
  }
  # psat > 140: qi = 47 (nota da Figura 1, Apendice D CETESB 2023, p. 4).
  # O degrau ~74 -> 47 e previsto na fonte oficial (EQ-06). CPRH e IBEAS
  # transcrevem "47 + 3" = 50; prevalece o texto oficial.
  i <- which(psat > 140)
  if (length(i)) {
    out[i] <- 47
  }

  pmin(100, pmax(0, out))
}

#' Qi de Coliformes Termotolerantes / E. coli (CETESB oficial)
#' @param col Concentracao de coliformes (NMP/100 mL).
#' @param microbial_type "thermotolerant_coliforms" ou "e_coli" (aplica fator 1.25x).
#' @return Vetor numerico com Qi (0-100).
#' @keywords internal
#' @noRd
.qi_cetesb_coliformes <- function(col, microbial_type = c("thermotolerant_coliforms", "e_coli")) {
  if (is.null(col)) return(NULL)
  microbial_type <- match.arg(microbial_type)
  col <- as.numeric(col)

  # E. coli x 1.25 -> equivalente em termotolerantes (Apendice D CETESB 2023,
  # p. 5: estudo CETESB 2008, ~80 E. coli para cada 100 termotolerantes).
  if (microbial_type == "e_coli") {
    col <- col * 1.25
  }

  # log10(C) com piso em 1 para contagens zero/positivas
  c_eff <- pmax(col, 0)
  logC  <- log10(pmax(c_eff, 1))

  out <- rep(NA_real_, length(col))

  # col <= 1 -> Qi = 100
  i <- which(c_eff <= 1)
  if (length(i)) out[i] <- 100

  # 1 < col <= 10 (0 < logC <= 1)
  i <- which(c_eff > 1 & logC <= 1)
  if (length(i)) {
    out[i] <- 100 - 33 * logC[i]
  }

  # 10 < col <= 100000 (1 < logC <= 5)
  i <- which(logC > 1 & logC <= 5)
  if (length(i)) {
    out[i] <- 100 - 37.2 * logC[i] + 3.60743 * (logC[i]^2)
  }

  # col > 100000 (logC > 5)
  i <- which(logC > 5)
  if (length(i)) {
    out[i] <- 3
  }

  pmin(100, pmax(0, out))
}

#' Qi de pH (CETESB oficial)
#' @param ph Valor de pH.
#' @return Vetor numerico com Qi (0-100).
#' @keywords internal
#' @noRd
.qi_cetesb_ph <- function(ph) {
  if (is.null(ph)) return(NULL)
  ph <- as.numeric(ph)
  out <- rep(NA_real_, length(ph))

  # ph <= 2
  i <- which(ph <= 2)
  if (length(i)) out[i] <- 2

  # 2 < ph <= 4
  i <- which(ph > 2 & ph <= 4)
  if (length(i)) out[i] <- 13.6 - 10.6 * ph[i] + 2.4364 * (ph[i]^2)

  # 4 < ph <= 6.2
  i <- which(ph > 4 & ph <= 6.2)
  if (length(i)) out[i] <- 155.5 - 77.36 * ph[i] + 10.2481 * (ph[i]^2)

  # 6.2 < ph <= 7
  i <- which(ph > 6.2 & ph <= 7)
  if (length(i)) out[i] <- -657.2 + 197.38 * ph[i] - 12.9167 * (ph[i]^2)

  # 7 < ph <= 8
  i <- which(ph > 7 & ph <= 8)
  if (length(i)) out[i] <- -427.8 + 142.05 * ph[i] - 9.695 * (ph[i]^2)

  # 8 < ph <= 8.5
  i <- which(ph > 8 & ph <= 8.5)
  if (length(i)) out[i] <- 216 - 16 * ph[i]

  # 8.5 < ph <= 9: exp natural (v0.10.0 usava 10^x, com salto 80 -> 0.0002 em 8.5).
  # Com exp() o trecho vale 80.0 em 8.5 e 45.0 em 9, compativel com a curva
  # de q2 (Apendice D CETESB 2023, Figura 1; leitura visual) (EQ-02).
  i <- which(ph > 8.5 & ph <= 9)
  if (length(i)) out[i] <- 1415823 * exp(-1.1507 * ph[i])

  # 9 < ph <= 10: 288 - 27*pH (v0.10.0 usava 50 - 32*(pH - 9), salto 45 -> 50 em 9).
  # Fontes secundarias divergem: CPRH transcreve "228 - 27*pH" (daria qi < 0
  # em todo o trecho; com 288 fecha em 45 e 18, os vizinhos); von Sperling via
  # IBEAS da "50 - 32*(pH - 9)" (salto +5 em pH 9, contrario a curva q2, que e
  # decrescente apos o pico). Adotado 288 - 27*pH; aprovacao do mantenedor
  # pendente (EQ-03).
  i <- which(ph > 9 & ph <= 10)
  if (length(i)) out[i] <- 288 - 27 * ph[i]

  # 10 < ph <= 12
  i <- which(ph > 10 & ph <= 12)
  if (length(i)) out[i] <- 633 - 106.5 * ph[i] + 4.5 * (ph[i]^2)

  # ph > 12
  i <- which(ph > 12)
  if (length(i)) out[i] <- 3

  pmin(100, pmax(0, out))
}

#' Qi de DBO5 (CETESB oficial)
#' @param dbo Concentracao de DBO em mg/L.
#' @return Vetor numerico com Qi (0-100).
#' @keywords internal
#' @noRd
.qi_cetesb_dbo <- function(dbo) {
  if (is.null(dbo)) return(NULL)
  C <- pmax(as.numeric(dbo), 0)
  out <- rep(NA_real_, length(C))

  # 0 <= C <= 5 (exp natural, coeficiente 99.96)
  i <- which(C >= 0 & C <= 5)
  if (length(i)) out[i] <- 99.96 * exp(-0.1232728 * C[i])

  # 5 < C <= 15: ln natural (v0.10.0 usava log10, com saltos em 5 e 15 mg/L).
  # Com ln, DBO 10 -> 32 (curva q3 da Figura 1: ~30); com log10 dava 73 (EQ-01).
  # CPRH escreve "log" (base ambigua); von Sperling via IBEAS escreve LOG10 e
  # a planilha do IBEAS da DBO 12 -> 70.6 (log10). Prevalece a curva oficial.
  i <- which(C > 5 & C <= 15)
  if (length(i)) out[i] <- 104.67 - 31.5463 * log(C[i])

  # 15 < C <= 30
  i <- which(C > 15 & C <= 30)
  if (length(i)) out[i] <- 4394.91 * (C[i]^(-1.99809))

  # C > 30
  i <- which(C > 30)
  if (length(i)) out[i] <- 2

  pmin(100, pmax(0, out))
}

#' Qi de Nitrogenio Total (CETESB oficial)
#' @param nt Concentracao de Nitrogenio Total em mg/L.
#' @return Vetor numerico com Qi (0-100).
#' @keywords internal
#' @noRd
.qi_cetesb_nt <- function(nt) {
  if (is.null(nt)) return(NULL)
  C <- pmax(as.numeric(nt), 0)
  out <- rep(NA_real_, length(C))

  # 0 <= C <= 10
  i <- which(C >= 0 & C <= 10)
  if (length(i)) out[i] <- 100 - 8.169 * C[i] + 0.3059 * (C[i]^2)

  # 10 < C <= 60: ln natural (v0.10.0 usava log10, com saltos em 10 e 60 mg/L).
  # Com ln, NT 20 -> 33 (curva q4 da Figura 1: ~32); com log10 dava 72 (EQ-04).
  # Mesmo conflito de base de log que na DBO (CPRH "log"; IBEAS LOG10).
  i <- which(C > 10 & C <= 60)
  if (length(i)) out[i] <- 101.9 - 23.1023 * log(C[i])

  # 60 < C <= 100 (exp natural)
  i <- which(C > 60 & C <= 100)
  if (length(i)) out[i] <- 159.3148 * exp(-0.0512842 * C[i])

  # C > 100: qi = 1 (nota da Figura 1, Apendice D CETESB 2023). O trecho
  # exponencial vale ~0.945 em 100 mg/L; o degrau de ~0.055 vem do ajuste (EQ-05).
  i <- which(C > 100)
  if (length(i)) out[i] <- 1

  pmin(100, pmax(0, out))
}

#' Qi de Fosforo Total (CETESB oficial com conversao P -> PO4)
#' @param pt Concentracao de Fosforo Total (mg/L).
#' @param phosphorus_basis "P" (default, aplica fator 3.066 para PO4) ou "PO4".
#' @return Vetor numerico com Qi (0-100).
#' @keywords internal
#' @noRd
.qi_cetesb_pt <- function(pt, phosphorus_basis = c("P", "PO4")) {
  if (is.null(pt)) return(NULL)
  phosphorus_basis <- match.arg(phosphorus_basis)
  pt <- pmax(as.numeric(pt), 0)

  # Se informado como P elementar, converte estequiometricamente para PO4
  C <- if (phosphorus_basis == "P") pt * 3.066 else pt
  out <- rep(NA_real_, length(C))

  # 0 <= C <= 1 (exp natural)
  i <- which(C >= 0 & C <= 1)
  if (length(i)) out[i] <- 99.0 * exp(-0.91629 * C[i])

  # 1 < C <= 5
  i <- which(C > 1 & C <= 5)
  if (length(i)) out[i] <- 57.6 - 20.178 * C[i] + 2.1326 * (C[i]^2)

  # 5 < C <= 10 (exp natural)
  i <- which(C > 5 & C <= 10)
  if (length(i)) out[i] <- 19.8 * exp(-0.13544 * C[i])

  # C > 10
  i <- which(C > 10)
  if (length(i)) out[i] <- 5

  pmin(100, pmax(0, out))
}

#' Qi de Turbidez (CETESB oficial)
#' @param turb Turbidez em NTU.
#' @return Vetor numerico com Qi (0-100).
#' @keywords internal
#' @noRd
.qi_cetesb_turbidez <- function(turb) {
  if (is.null(turb)) return(NULL)
  T_val <- pmax(as.numeric(turb), 0)
  out <- rep(NA_real_, length(T_val))

  # 0 <= T <= 25
  i <- which(T_val >= 0 & T_val <= 25)
  if (length(i)) out[i] <- 100.17 - 2.67 * T_val[i] + 0.03775 * (T_val[i]^2)

  # 25 < T <= 100 (exp natural)
  i <- which(T_val > 25 & T_val <= 100)
  if (length(i)) out[i] <- 84.76 * exp(-0.016206 * T_val[i])

  # T > 100
  i <- which(T_val > 100)
  if (length(i)) out[i] <- 5

  pmin(100, pmax(0, out))
}

#' Qi de Solidos Totais (Residuo Total - CETESB oficial)
#' @param st Solidos Totais / Residuo Total em mg/L.
#' @return Vetor numerico com Qi (0-100).
#' @keywords internal
#' @noRd
.qi_cetesb_solidos_totais <- function(st) {
  if (is.null(st)) return(NULL)
  C <- pmax(as.numeric(st), 0)
  out <- rep(NA_real_, length(C))

  # 0 <= C <= 150
  i <- which(C >= 0 & C <= 150)
  if (length(i)) out[i] <- 79.75 + 0.166 * C[i] - 0.001088 * (C[i]^2)

  # 150 < C <= 500
  i <- which(C > 150 & C <= 500)
  if (length(i)) out[i] <- 101.67 - 0.13917 * C[i]

  # C > 500
  i <- which(C > 500)
  if (length(i)) out[i] <- 32

  pmin(100, pmax(0, out))
}

#' Qi de Variacao de Temperatura / Delta T (CETESB oficial)
#' @param delta_t Variacao de temperatura (graus Celsius).
#' @param temperature_method "cetesb_default" (retorna Qi=94), "delta" ou "reference".
#' @param temp_water Temperatura da agua (opcional, quando temperature_method="reference").
#' @param temp_ref Temperatura de referencia (opcional).
#' @return Vetor numerico com Qi (0-100).
#' @keywords internal
#' @noRd
.qi_cetesb_temperatura <- function(
  delta_t = NULL,
  temperature_method = c("cetesb_default", "delta", "reference"),
  temp_water = NULL,
  temp_ref = NULL,
  n = 1
) {
  temperature_method <- match.arg(temperature_method)

  if (temperature_method == "cetesb_default" || (is.null(delta_t) && is.null(temp_ref))) {
    return(rep(94, n))
  }

  if (temperature_method == "reference" && !is.null(temp_water) && !is.null(temp_ref)) {
    delta_t <- as.numeric(temp_water) - as.numeric(temp_ref)
  }

  if (is.null(delta_t)) {
    return(rep(94, n))
  }

  dT <- as.numeric(delta_t)
  out <- rep(NA_real_, length(dT))

  # dT < -5: qi indefinido (CETESB 2023, Apendice D, Figura 1, p. 4).
  # Ate a v0.10.0 o pacote retornava 93.
  # ATENCAO (EQ-07/08): os trechos abaixo NAO acompanham a curva de q6 da
  # Figura 1 (pico ~93 em dT = 0; ~60 em 5; ~30 em 10; ~12 em 15). Nao foram
  # corrigidos porque a fonte so traz a curva grafica, sem equacao.
  i <- which(dT < -5)
  if (length(i)) {
    warning(length(i), " delta temperature value(s) below -5 C: qi is undefined ",
            "(CETESB Appendix D, Figure 1) and was set to NA.", call. = FALSE)
  }

  # -5 <= dT <= 0
  i <- which(dT >= -5 & dT <= 0)
  if (length(i)) out[i] <- 100 - 1.4 * abs(dT[i]) - 0.04 * (dT[i]^2)

  # 0 < dT <= 5
  i <- which(dT > 0 & dT <= 5)
  if (length(i)) out[i] <- 100 - 3.8 * dT[i] + 0.1 * (dT[i]^2)

  # 5 < dT <= 15
  i <- which(dT > 5 & dT <= 15)
  if (length(i)) out[i] <- 90.5 - 4.5 * (dT[i] - 5)

  # dT > 15
  i <- which(dT > 15)
  if (length(i)) out[i] <- 9

  pmin(100, pmax(0, out))
}

#' Marca valores fisicamente invalidos como NA, com aviso
#' @param x Vetor (numerico ou nao; nao numerico e devolvido sem alteracao).
#' @param name Nome do parametro, usado na mensagem.
#' @param lower,upper Limites fisicos inclusivos.
#' @return Vetor com valores invalidos substituidos por NA.
#' @keywords internal
#' @noRd
.iqa_flag_invalid <- function(x, name, lower = -Inf, upper = Inf) {
  if (!is.numeric(x)) return(x)
  bad <- !is.na(x) & (!is.finite(x) | x < lower | x > upper)
  if (any(bad)) {
    warning(
      sum(bad), " physically invalid value(s) in '", name, "' (outside [",
      lower, ", ", upper, "] or non-finite) set to NA.",
      call. = FALSE
    )
    x[bad] <- NA_real_
  }
  x
}

# Pesos oficiais CETESB (soma = 1.0), ordem canonica dos 9 componentes
.iqa_cetesb_weights <- c(
  od                         = 0.17,
  coliformes_termotolerantes = 0.15,
  ph                         = 0.12,
  dbo                        = 0.10,
  nt_total                   = 0.10,
  p_total                    = 0.10,
  temperatura                = 0.10,
  turbidez                   = 0.08,
  solidos_totais             = 0.08
)

# Sufixos das colunas de componentes, na mesma ordem de .iqa_cetesb_weights
.iqa_comp_suffix <- c("od", "colif", "ph", "dbo", "nt", "pt", "temp", "turb", "st")

# ------------------------------------------------------------------------------
# Camada de Auditoria dos Componentes do IQA
# ------------------------------------------------------------------------------

#' Auditoria detalhada dos subindices do IQA (metodo CETESB)
#'
#' @description
#' Retorna uma tabela estruturada contendo o valor bruto, valor transformado,
#' subindice Qi, peso Wi e parcela ponderada \eqn{Qi^{Wi}} para cada um dos 9
#' componentes oficiais do IQA CETESB.
#'
#' @param df Data frame de entrada com os dados de monitoramento.
#' @param altitude_m Altitude em metros (default 0).
#' @param phosphorus_basis "P" (default) ou "PO4".
#' @param microbial_type "thermotolerant_coliforms" (default) ou "e_coli".
#' @param temperature_method "cetesb_default" (default), "delta" ou "reference".
#' @param temperature_reference Vetor ou escalar com temperatura de referencia.
#' @param delta_temperature Vetor ou escalar com a variacao de temperatura (Delta T em graus Celsius).
#' @param censor_policy Politica de tratamento de dados censurados ("limit", "half_limit", "zero", "na", "preserve").
#' @param method Motor de calculo: \code{"CETESB"} (padrao) ou \code{"IGAM"}.
#'   Para o IGAM os componentes sao obtidos com
#'   \code{iqa(df, method = "IGAM", details = TRUE)}; aqui \code{"IGAM"} gera
#'   erro com essa orientacao.
#'
#' @return Um tibble ou data frame detalhado com colunas para cada parametro e o IQA calculado.
#' @export
#' @examples
#' d <- data.frame(
#'   od = 6.5, temperatura = 25, dbo = 2, coliformes = 200,
#'   ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8,
#'   solidos_totais = 120
#' )
#' iqa_components(d)
iqa_components <- function(
  df,
  altitude_m = 0,
  phosphorus_basis = c("P", "PO4"),
  microbial_type = c("thermotolerant_coliforms", "e_coli"),
  temperature_method = c("cetesb_default", "delta", "reference"),
  temperature_reference = NULL,
  delta_temperature = NULL,
  censor_policy = c("limit", "half_limit", "zero", "na", "preserve"),
  method = c("CETESB", "IGAM")
) {
  method <- match.arg(method)
  if (method == "IGAM") {
    if (!isTRUE(.iqa_igam_spec$available)) {
      stop(.iqa_method_unavailable("IGAM", .iqa_igam_spec$blocked_reason, df))
    }
    stop("For method = 'IGAM' use iqa(df, method = 'IGAM', details = TRUE): ",
         "the components (qi, effective weights, unrounded IQA) are in ",
         "attr(, 'components'). IGAM needs arguments (e.g. nitrate_unit) that ",
         "iqa_components() does not take.", call. = FALSE)
  }
  phosphorus_basis   <- match.arg(phosphorus_basis)
  microbial_type     <- match.arg(microbial_type)
  temperature_method <- match.arg(temperature_method)
  censor_policy      <- match.arg(censor_policy)

  n_rows <- nrow(df)
  if (n_rows == 0L) return(tibble::tibble())

  # Validacao de altitude_m
  if (!is.numeric(altitude_m) || any(!is.finite(altitude_m))) {
    stop("altitude_m must be a finite numeric value or vector.")
  }
  if (length(altitude_m) != 1L && length(altitude_m) != n_rows) {
    stop("altitude_m must have length 1 or match nrow(df) (", n_rows, ").")
  }
  # Dominio fisico da correcao barometrica (1 - 2.28675e-5 * h)^5.167:
  # a base deve ser positiva; aceitamos de -500 m (depressoes) a 9000 m.
  if (any(altitude_m < -500 | altitude_m > 9000)) {
    stop("altitude_m must be between -500 and 9000 m (domain of the barometric correction).")
  }
  if (length(altitude_m) == 1L && n_rows > 1L) altitude_m <- rep(altitude_m, n_rows)

  weights <- .iqa_cetesb_weights

  # Resolucao de colunas
  col_od     <- .resolve_col(df, "od")
  col_temp   <- .resolve_col(df, "temperatura")
  col_deltaT <- .resolve_col(df, "delta_temperatura")
  col_ph     <- .resolve_col(df, "ph")
  col_dbo    <- .resolve_col(df, "dbo")
  col_colif  <- if (microbial_type == "e_coli") .resolve_col(df, "e_coli") else .resolve_col(df, "coliformes_termotolerantes")
  if (is.null(col_colif)) col_colif <- .resolve_col(df, "coliformes_termotolerantes")
  col_nt     <- .resolve_col(df, "nt_total")
  col_pt     <- .resolve_col(df, "p_total")
  col_turb   <- .resolve_col(df, "turbidez")
  col_st     <- .resolve_col(df, "solidos_totais")

  # Resolucao de delta_temperature (precedencia: argumento explicito > coluna do df)
  v_deltaT <- if (!is.null(delta_temperature)) {
    if (!is.numeric(delta_temperature) || any(!is.finite(delta_temperature))) {
      stop("delta_temperature must be a finite numeric value or vector.")
    }
    if (length(delta_temperature) != 1L && length(delta_temperature) != n_rows) {
      stop("delta_temperature must have length 1 or match nrow(df) (", n_rows, ").")
    }
    if (length(delta_temperature) == 1L && n_rows > 1L) {
      rep(as.numeric(delta_temperature), n_rows)
    } else {
      as.numeric(delta_temperature)
    }
  } else if (!is.null(col_deltaT)) {
    .parse_nd_ld(df[[col_deltaT]], censor_policy)
  } else {
    NULL
  }

  if (temperature_method == "delta" && is.null(v_deltaT)) {
    stop("temperature_method = 'delta' requires argument 'delta_temperature' or a 'delta_temperatura' column in df.")
  }

  if (temperature_method == "reference") {
    if (is.null(temperature_reference)) {
      stop("temperature_method = 'reference' requires argument 'temperature_reference'.")
    }
    if (!is.numeric(temperature_reference) || any(!is.finite(temperature_reference))) {
      stop("temperature_reference must be a finite numeric value or vector.")
    }
    if (length(temperature_reference) != 1L && length(temperature_reference) != n_rows) {
      stop("temperature_reference must have length 1 or match nrow(df) (", n_rows, ").")
    }
    if (is.null(col_temp)) {
      stop("temperature_method = 'reference' requires water temperature column ('temperatura') in df.")
    }
  }

  if (temperature_method != "cetesb_default") {
    warning("The package's delta-T sub-index curve does not match the CETESB ",
            "q6 curve (Appendix D 2023, Figure 1); temperature_method = '",
            temperature_method, "' results are not reliable. See ",
            "inst/audit/IQA_AUDITORIA.md (EQ-07/08).", call. = FALSE)
  }

  # Extrai e trata valores numericos aplicando politica de censura
  v_od     <- if (!is.null(col_od))     .parse_nd_ld(df[[col_od]], censor_policy)     else rep(NA_real_, n_rows)
  v_temp   <- if (!is.null(col_temp))   .parse_nd_ld(df[[col_temp]], censor_policy)   else rep(NA_real_, n_rows)
  v_ph     <- if (!is.null(col_ph))     .parse_nd_ld(df[[col_ph]], censor_policy)     else rep(NA_real_, n_rows)
  v_dbo    <- if (!is.null(col_dbo))    .parse_nd_ld(df[[col_dbo]], censor_policy)    else rep(NA_real_, n_rows)
  v_colif  <- if (!is.null(col_colif))  .parse_nd_ld(df[[col_colif]], censor_policy)  else rep(NA_real_, n_rows)
  v_nt     <- if (!is.null(col_nt))     .parse_nd_ld(df[[col_nt]], censor_policy)     else rep(NA_real_, n_rows)
  v_pt     <- if (!is.null(col_pt))     .parse_nd_ld(df[[col_pt]], censor_policy)     else rep(NA_real_, n_rows)
  v_turb   <- if (!is.null(col_turb))   .parse_nd_ld(df[[col_turb]], censor_policy)   else rep(NA_real_, n_rows)
  v_st     <- if (!is.null(col_st))     .parse_nd_ld(df[[col_st]], censor_policy)     else rep(NA_real_, n_rows)

  # Valores fisicamente impossiveis (negativos, pH fora de 0-14, Inf) viram NA
  # com aviso, em vez de serem truncados silenciosamente por pmax().
  v_od    <- .iqa_flag_invalid(v_od,    "od",             lower = 0)
  v_temp  <- .iqa_flag_invalid(v_temp,  "temperatura",    lower = -2, upper = 50)
  v_ph    <- .iqa_flag_invalid(v_ph,    "ph",             lower = 0,  upper = 14)
  v_dbo   <- .iqa_flag_invalid(v_dbo,   "dbo",            lower = 0)
  v_colif <- .iqa_flag_invalid(v_colif, "coliformes",     lower = 0)
  v_nt    <- .iqa_flag_invalid(v_nt,    "nt_total",       lower = 0)
  v_pt    <- .iqa_flag_invalid(v_pt,    "p_total",        lower = 0)
  v_turb  <- .iqa_flag_invalid(v_turb,  "turbidez",       lower = 0)
  v_st    <- .iqa_flag_invalid(v_st,    "solidos_totais", lower = 0)

  # Calculo de Qi para cada componente
  qi_od   <- .qi_cetesb_od(v_od, v_temp, altitude_m)
  qi_col  <- .qi_cetesb_coliformes(v_colif, microbial_type)
  qi_ph   <- .qi_cetesb_ph(v_ph)
  qi_dbo  <- .qi_cetesb_dbo(v_dbo)
  qi_nt   <- .qi_cetesb_nt(v_nt)
  qi_pt   <- .qi_cetesb_pt(v_pt, phosphorus_basis)
  qi_turb <- .qi_cetesb_turbidez(v_turb)
  qi_st   <- .qi_cetesb_solidos_totais(v_st)
  qi_temp <- .qi_cetesb_temperatura(
    delta_t = v_deltaT,
    temperature_method = temperature_method,
    temp_water = v_temp,
    temp_ref = temperature_reference,
    n = n_rows
  )

  # Monta dataframe de componentes
  comp_df <- tibble::tibble(
    raw_od     = v_od,     qi_od     = qi_od,     wi_od     = weights[["od"]],     term_od     = qi_od ^ weights[["od"]],
    raw_colif  = v_colif,  qi_colif  = qi_col,    wi_colif  = weights[["coliformes_termotolerantes"]], term_colif = qi_col ^ weights[["coliformes_termotolerantes"]],
    raw_ph     = v_ph,     qi_ph     = qi_ph,     wi_ph     = weights[["ph"]],     term_ph     = qi_ph ^ weights[["ph"]],
    raw_dbo    = v_dbo,    qi_dbo    = qi_dbo,    wi_dbo    = weights[["dbo"]],    term_dbo    = qi_dbo ^ weights[["dbo"]],
    raw_nt     = v_nt,     qi_nt     = qi_nt,     wi_nt     = weights[["nt_total"]],term_nt    = qi_nt ^ weights[["nt_total"]],
    raw_pt     = v_pt,     qi_pt     = qi_pt,     wi_pt     = weights[["p_total"]], term_pt     = qi_pt ^ weights[["p_total"]],
    raw_temp   = v_temp,   qi_temp   = qi_temp,   wi_temp   = weights[["temperatura"]], term_temp = qi_temp ^ weights[["temperatura"]],
    raw_turb   = v_turb,   qi_turb   = qi_turb,   wi_turb   = weights[["turbidez"]], term_turb   = qi_turb ^ weights[["turbidez"]],
    raw_st     = v_st,     qi_st     = qi_st,     wi_st     = weights[["solidos_totais"]], term_st     = qi_st ^ weights[["solidos_totais"]]
  )

  # IQA = prod(termos)
  terms_mat <- as.matrix(comp_df[, c("term_od", "term_colif", "term_ph", "term_dbo", "term_nt", "term_pt", "term_temp", "term_turb", "term_st")])
  comp_df$IQA <- apply(terms_mat, 1, prod)

  attr(comp_df, "iqa_method")             <- "CETESB"
  attr(comp_df, "iqa_censor_policy")      <- censor_policy
  attr(comp_df, "iqa_phosphorus_basis")   <- phosphorus_basis
  attr(comp_df, "iqa_microbial_type")     <- microbial_type
  attr(comp_df, "iqa_temperature_method") <- temperature_method
  attr(comp_df, "iqa_altitude_m")         <- altitude_m

  comp_df
}

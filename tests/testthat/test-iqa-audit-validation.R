# tests/testthat/test-iqa-audit-validation.R
# Testes da auditoria tecnico-cientifica do IQA
# (fronteiras, continuidade, coerencia da auditoria e integridade da API)
#
# Importante: os testes de fronteira dos qi verificam o comportamento da
# implementacao atual. Eles NAO comprovam equivalencia com o Apendice D da
# CETESB; essa conferencia esta pendente (inst/audit/IQA_AUDITORIA.md).

d1 <- function() {
  data.frame(
    od = 6.5, temperatura = 25, dbo = 2, coliformes = 200,
    ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8,
    solidos_totais = 120
  )
}

test_that("Fronteiras de pH (EQ-02, EQ-03)", {
  eps <- 1e-4

  # pH 8.5: 216 - 16*ph -> 1415823 * exp(-1.1507 * ph)
  expect_equal(.qi_cetesb_ph(8.5), 80.0, tolerance = 0.05)
  expect_equal(.qi_cetesb_ph(8.5 - eps), 80.0, tolerance = 0.1)
  expect_equal(.qi_cetesb_ph(8.5 + eps), 80.0, tolerance = 0.1)

  # pH 9.0: exp -> 288 - 27*ph
  expect_equal(.qi_cetesb_ph(9.0), 45.0, tolerance = 0.1)
  expect_equal(.qi_cetesb_ph(9.0 + eps), 45.0, tolerance = 0.1)

  # pH 10.0: 288 - 27*ph -> 633 - 106.5*ph + 4.5*ph^2
  expect_equal(.qi_cetesb_ph(10.0), 18.0, tolerance = 0.05)
  expect_equal(.qi_cetesb_ph(10.0 + eps), 18.0, tolerance = 0.1)

  # pH 12: trecho quadratico termina em 3, igual ao piso fixo
  expect_equal(.qi_cetesb_ph(12), 3)
  expect_equal(.qi_cetesb_ph(12 + eps), 3)
})

test_that("Fronteiras e monotonicidade de DBO (EQ-01)", {
  eps <- 1e-4

  expect_equal(.qi_cetesb_dbo(5.0), 53.97, tolerance = 0.05)
  expect_equal(.qi_cetesb_dbo(5.0 + eps), 53.90, tolerance = 0.1)

  dbo_seq <- seq(0, 35, by = 0.5)
  expect_true(all(diff(.qi_cetesb_dbo(dbo_seq)) <= 1e-6))

  expect_equal(.qi_cetesb_dbo(15.0), 19.24, tolerance = 0.05)
  expect_equal(.qi_cetesb_dbo(15.0 + eps), 19.63, tolerance = 0.05)
})

test_that("Fronteiras e monotonicidade de Nitrogenio Total (EQ-04, EQ-05)", {
  eps <- 1e-4

  expect_equal(.qi_cetesb_nt(10.0), 48.90, tolerance = 0.05)
  expect_equal(.qi_cetesb_nt(10.0 + eps), 48.70, tolerance = 0.1)
  expect_equal(.qi_cetesb_nt(60.0), 7.31, tolerance = 0.05)
  expect_equal(.qi_cetesb_nt(60.0 + eps), 7.34, tolerance = 0.05)

  # Monotonicidade decrescente em [0, 100] (dominio das curvas)
  expect_true(all(diff(.qi_cetesb_nt(seq(0, 100, by = 1))) <= 1e-6))

  # Degrau conhecido em 100 mg/L: trecho exp vale ~0.945, piso fixo vale 1.
  # Registrado como EQ-05; nao corrigido sem fonte primaria.
  expect_equal(.qi_cetesb_nt(100), 0.945, tolerance = 0.01)
  expect_equal(.qi_cetesb_nt(100 + eps), 1)
})

test_that("OD: degrau em psat > 140% mantido como na v0.10.0 (EQ-06)", {
  # Cs a 25 C e 0 m ~ 8.263 mg/L
  od_140 <- .qi_cetesb_od(8.263 * 1.40, temp = 25, altitude_m = 0)
  expect_true(od_140 > 70 && od_140 < 75)
  expect_equal(.qi_cetesb_od(8.263 * 1.41, temp = 25, altitude_m = 0), 47)
})

test_that("API: classify_iqa() suporta classification e scheme com depreciacao", {
  vals <- c(15, 30, 45, 60, 85)

  cls_cetesb <- classify_iqa(vals, classification = "cetesb")
  expect_s3_class(cls_cetesb, "ordered")
  expect_equal(as.character(cls_cetesb), c("Pessima", "Ruim", "Regular", "Boa", "Otima"))

  # IGAM 25/50/70/90: 45 -> Ruim (25, 50]; 60 -> Medio (50, 70]
  cls_igam <- classify_iqa(vals, classification = "igam")
  expect_equal(as.character(cls_igam), c("Muito Ruim", "Ruim", "Ruim", "Medio", "Bom"))

  # classify_iqa() sem argumentos = CETESB
  expect_equal(classify_iqa(vals), cls_cetesb)

  expect_warning(cls_leg <- classify_iqa(vals, scheme = "cetesb"), "deprecated")
  expect_equal(cls_leg, cls_cetesb)
  expect_warning(cls_leg2 <- classify_iqa(vals, scheme = "legacy"), "deprecated")
  expect_equal(as.character(cls_leg2)[1], "Muito ruim")

  # Ambos informados e iguais: apenas aviso
  expect_warning(classify_iqa(vals, classification = "igam", scheme = "igam"), "deprecated")
  # Ambos informados e diferentes: erro
  expect_error(
    classify_iqa(vals, classification = "cetesb", scheme = "igam"),
    "Conflicting arguments"
  )
})

test_that("classify_iqa(): fronteiras exatas, eps e valores especiais", {
  eps <- 1e-6
  cut_test <- function(cl, cuts, labs) {
    for (k in seq_along(cuts)) {
      b <- cuts[k]
      got <- as.character(classify_iqa(c(b - eps, b, b + eps), classification = cl))
      expect_equal(got, c(labs[k], labs[k], labs[k + 1]), info = paste(cl, b))
    }
  }
  cut_test("cetesb", c(19, 36, 51, 79), c("Pessima", "Ruim", "Regular", "Boa", "Otima"))
  cut_test("ana",    c(19, 36, 51, 79), c("Pessima", "Ruim", "Regular", "Boa", "Otima"))
  cut_test("igam",   c(25, 50, 70, 90), c("Muito Ruim", "Ruim", "Medio", "Bom", "Excelente"))
  cut_test("legacy", c(25, 50, 70, 90), c("Muito ruim", "Ruim", "Regular", "Boa", "Otima"))

  expect_equal(as.character(classify_iqa(c(0, 100))), c("Pessima", "Otima"))
  expect_true(all(is.na(classify_iqa(c(NA, NaN)))))
  expect_warning(r <- classify_iqa(c(-1, 101, Inf)), "outside")
  expect_true(all(is.na(r)))
  expect_equal(
    as.character(classify_iqa(c(10, 95), classification = "igam", locale = "en")),
    c("Very Poor", "Excellent")
  )
  # Sem arredondamento: 19.004 nao vira 19
  expect_equal(as.character(classify_iqa(19.004)), "Ruim")
  expect_length(classify_iqa(numeric(0)), 0)
})

test_that("iqa(): escala segue method; classify_iqa() reclassifica sem mudar o numero", {
  d <- data.frame(
    od = c(6.5, 3), temperatura = 25, dbo = c(2, 8), coliformes = c(200, 5000),
    ph = c(7.2, 8.7), nt_total = c(1.2, 15), p_total = c(0.05, 0.4),
    turbidez = c(8, 60), solidos_totais = c(120, 300)
  )
  out <- iqa(d)
  expect_equal(attr(out, "iqa_classification"), "cetesb")
  expect_equal(attr(out, "iqa_calculation_method"), "CETESB")
  expect_identical(out$IQA_status, classify_iqa(attr(iqa(d, details = TRUE), "components")$IQA_unrounded))
  for (cl in c("cetesb", "igam", "ana", "legacy")) {
    expect_length(classify_iqa(out$IQA, classification = cl), nrow(d))
  }
  expect_identical(iqa(d)$IQA, out$IQA)
})

test_that("API: classification e scheme foram removidos de iqa(); na_rm segue depreciado", {
  d <- d1()
  # 'classification' existia na 0.10.0 (cetesb/legacy): erro com orientacao
  expect_error(iqa(d, classification = "legacy"), "removed from iqa.*classify_iqa")
  expect_error(iqa(d, classification = "cetesb"), "removed from iqa")
  expect_error(iqa(d, scheme = "igam"), "removed from iqa")
  # Chamada posicional da 0.10.0: iqa(df, method, classification)
  expect_error(iqa(d, "CETESB", "legacy"), "third positional argument.*classify_iqa")
  expect_false("classification" %in% names(formals(iqa)))
  # classify_iqa() mantem classification e o scheme legado (com aviso)
  expect_equal(as.character(classify_iqa(80, classification = "igam")), "Bom")
  expect_warning(classify_iqa(80, scheme = "legacy"), "deprecated")
  expect_warning(o2 <- iqa(d[, -3], na_rm = TRUE), "deprecated")
  expect_false(is.na(o2$IQA))
  expect_warning(iqa(d, alow_partial = TRUE), "Unrecognized")
  expect_false(any(grepl("cross", attr(iqa(d), "iqa_nonstandard"))))
})

test_that("API: iqa() encaminha delta_temperature e valida temperature_method", {
  d <- d1()

  expect_error(iqa(d, temperature_method = "delta"), "requires argument 'delta_temperature'")

  comps_dT <- attr(suppressWarnings(iqa(d, temperature_method = "delta", delta_temperature = 2, details = TRUE)), "components")
  expect_equal(comps_dT$qi_temp, 92.8, tolerance = 0.01)

  expect_error(iqa(d, temperature_method = "reference"), "requires argument 'temperature_reference'")
})

test_that("temperature_method = 'reference' calcula delta a partir da referencia", {
  d <- d1()
  cp_ref <- attr(suppressWarnings(iqa(d, temperature_method = "reference", temperature_reference = 23, details = TRUE)), "components")
  cp_dlt <- attr(suppressWarnings(iqa(d, temperature_method = "delta", delta_temperature = 2, details = TRUE)), "components")
  expect_equal(cp_ref$qi_temp, cp_dlt$qi_temp)
  expect_error(suppressWarnings(iqa(d, temperature_method = "reference", temperature_reference = c(1, 2))), "match nrow")

  # Coluna delta_temperatura e usada quando o argumento nao e informado
  d$delta_temperatura <- 2
  cp_col <- attr(suppressWarnings(iqa(d, temperature_method = "delta", details = TRUE)), "components")
  expect_equal(cp_col$qi_temp, cp_dlt$qi_temp)
  # Argumento tem precedencia sobre a coluna
  cp_arg <- attr(suppressWarnings(iqa(d, temperature_method = "delta", delta_temperature = 0, details = TRUE)), "components")
  expect_equal(cp_arg$qi_temp, 100)
})

test_that("API: iqa() valida pesos customizados e grava pesos efetivos", {
  d <- d1()

  expect_error(iqa(d, pesos = c(od = -0.5)), "must be non-negative")
  expect_error(iqa(d, pesos = c(od = Inf)), "must be non-negative")
  expect_error(
    iqa(d, pesos = c(od = 0, coliformes = 0, ph = 0, dbo = 0, nt_total = 0,
                     p_total = 0, temperatura = 0, turbidez = 0, solidos_totais = 0)),
    "greater than zero"
  )
  expect_error(iqa(d, pesos = c(tds = 0.1)), "Unknown parameter")
  expect_error(iqa(d, pesos = c(0.1, 0.2)), "named numeric")
  expect_error(iqa(d, pesos = c(od = 0.1, do = 0.2)), "Duplicated")

  custom_w <- c(od = 0.30, dbo = 0.30, ph = 0.40)
  out_p <- iqa(d, pesos = custom_w, details = TRUE)
  comps_p <- attr(out_p, "components")
  w_nom <- .iqa_cetesb_weights
  w_nom[c("od", "dbo", "ph")] <- c(0.30, 0.30, 0.40)
  w_eff <- w_nom / sum(w_nom)
  expect_equal(comps_p$wi_od[1], unname(w_eff["od"]))
  expect_equal(comps_p$wi_ph[1], unname(w_eff["ph"]))
  expect_equal(sum(unlist(comps_p[1, paste0("wi_", .iqa_comp_suffix)])), 1)
  expect_equal(comps_p$IQA[1], out_p$IQA[1])
  expect_true(attr(out_p, "iqa_custom_weights"))
  expect_true("custom_weights" %in% attr(out_p, "iqa_nonstandard"))
  expect_equal(attr(out_p, "iqa_weights")[["od"]], 0.30)

  # Sem pesos customizados: resultado padrao, sem marca de nao-padrao
  out_std <- iqa(d)
  expect_false(attr(out_std, "iqa_custom_weights"))
  expect_length(attr(out_std, "iqa_nonstandard"), 0)
})

test_that("Auditoria: IQA recomputado de qi e wi efetivos reproduz o valor retornado", {
  d <- data.frame(
    od = c(6.5, 3, 9), temperatura = c(25, 22, 18), dbo = c(2, 8, NA),
    coliformes = c(200, 5000, 30), ph = c(7.2, 8.7, 6.5), nt_total = c(1.2, 15, 0.4),
    p_total = c(0.05, 0.4, 0.02), turbidez = c(8, 60, 3), solidos_totais = c(120, 300, 60)
  )
  for (pesos in list(NULL, c(od = 0.4, turbidez = 0))) {
    for (ap in c(FALSE, TRUE)) {
      out <- iqa(d, details = TRUE, allow_partial = ap, pesos = pesos)
      cp <- attr(out, "components")
      qi <- as.matrix(cp[, paste0("qi_", .iqa_comp_suffix)])
      wi <- as.matrix(cp[, paste0("wi_", .iqa_comp_suffix)])
      tm <- as.matrix(cp[, paste0("term_", .iqa_comp_suffix)])
      recomputed <- vapply(seq_len(nrow(d)), function(i) {
        ok <- wi[i, ] > 0
        prod(qi[i, ok] ^ wi[i, ok])
      }, numeric(1))
      valid <- !is.na(out$IQA)
      expect_equal(recomputed[valid], cp$IQA_unrounded[valid], tolerance = 1e-10)
      expect_equal(apply(tm, 1, prod)[valid], cp$IQA_unrounded[valid], tolerance = 1e-10)
      expect_equal(round(cp$IQA_unrounded, 2), out$IQA)
    }
  }

  # Linha 3 (sem DBO): NA no modo estrito
  strict <- iqa(d, details = TRUE)
  expect_true(is.na(strict$IQA[3]))
  expect_true(attr(strict, "iqa_complete"))

  # Modo parcial: linha registrada como estimativa parcial
  part <- iqa(d, details = TRUE, allow_partial = TRUE)
  cp <- attr(part, "components")
  expect_false(is.na(part$IQA[3]))
  expect_identical(attr(part, "iqa_partial_rows"), 3L)
  expect_false(attr(part, "iqa_complete"))
  expect_true("partial_rows" %in% attr(part, "iqa_nonstandard"))
  expect_true(cp$IQA_partial[3])
  expect_equal(cp$missing_params[3], "dbo")
  expect_equal(cp$wi_dbo[3], 0)
  expect_equal(sum(unlist(cp[3, paste0("wi_", .iqa_comp_suffix)])), 1)
})

test_that("Valores fisicamente invalidos viram NA com aviso", {
  d <- d1()
  d$dbo <- -2
  d$ph <- 15
  d$turbidez <- Inf
  w <- character(0)
  cp <- withCallingHandlers(
    iqa_components(d),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("'dbo'", w)))
  expect_true(any(grepl("'ph'", w)))
  expect_true(any(grepl("'turbidez'", w)))
  expect_true(is.na(cp$qi_dbo) && is.na(cp$qi_ph) && is.na(cp$qi_turb))
  expect_true(is.na(suppressWarnings(iqa(d))$IQA))
})

test_that("API: iqa() valida altitude_m", {
  d <- d1()

  expect_error(iqa(d, altitude_m = "500"), "finite numeric")
  expect_error(iqa(d, altitude_m = c(100, 200)), "match nrow")
  expect_error(iqa(d, altitude_m = 50000), "between -500 and 9000")

  # Altitude por ponto: cada linha usa a sua altitude
  d2 <- d[c(1, 1), ]
  cp <- attr(iqa(d2, altitude_m = c(0, 1500), details = TRUE), "components")
  expect_equal(cp$qi_od[1], .qi_cetesb_od(6.5, 25, 0))
  expect_equal(cp$qi_od[2], .qi_cetesb_od(6.5, 25, 1500))
  expect_false(isTRUE(all.equal(cp$qi_od[1], cp$qi_od[2])))

  # Altitude negativa (abaixo do nivel do mar) e aceita
  expect_false(is.na(iqa(d, altitude_m = -100)$IQA))
})

test_that("Formatos de entrada: vazio, tibble e censurados", {
  d0 <- data.frame(od = numeric(0), ph = numeric(0))
  out0 <- iqa(d0)
  expect_equal(nrow(out0), 0L)
  expect_s3_class(out0$IQA_status, "ordered")
  expect_equal(attr(out0, "iqa_classification"), "cetesb")
  expect_equal(attr(out0, "iqa_method"), "CETESB")

  d <- d1()
  expect_equal(iqa(tibble::as_tibble(d))$IQA, iqa(d)$IQA)

  dc <- d
  dc$dbo <- "<2"
  expect_equal(iqa(dc)$IQA, iqa(d)$IQA)
  expect_lt(iqa(dc, censor_policy = "half_limit")$IQA, Inf)
})

test_that("Rota legada registra metadados de nao-padrao", {
  data(wq_demo, package = "tikatuwq", envir = environment())
  out <- suppressWarnings(iqa(wq_demo, method = "CETESB_legacy_approx", allow_partial = TRUE))
  expect_equal(attr(out, "iqa_method"), "CETESB_legacy_approx")
  expect_true("legacy_approximate_curves" %in% attr(out, "iqa_nonstandard"))
})

test_that("Regras fixas do Apendice D CETESB 2023 (Figura 1, p. 4; p. 5)", {
  # Valores fixos fora do dominio das curvas (notas da Figura 1)
  expect_equal(.qi_cetesb_ph(1.5), 2)
  expect_equal(.qi_cetesb_ph(12.5), 3)
  expect_equal(.qi_cetesb_dbo(31), 2)
  expect_equal(.qi_cetesb_nt(101), 1)
  expect_equal(.qi_cetesb_pt(10.5, "PO4"), 5)
  expect_equal(.qi_cetesb_turbidez(101), 5)
  expect_equal(.qi_cetesb_solidos_totais(501), 32)
  expect_equal(.qi_cetesb_coliformes(2e5), 3)
  expect_equal(.qi_cetesb_od(8.263 * 1.5, temp = 25, altitude_m = 0), 47)
  expect_equal(.qi_cetesb_temperatura(16, "delta", n = 1), 9)

  # P total x 3,066 -> PO4 (p. 5)
  expect_equal(.qi_cetesb_pt(0.1, "P"), .qi_cetesb_pt(0.3066, "PO4"))
  # E. coli x 1,25 -> termotolerantes (p. 5)
  expect_equal(.qi_cetesb_coliformes(800, "e_coli"), .qi_cetesb_coliformes(1000))

  # Delta T < -5: qi indefinido -> NA com aviso
  expect_warning(q <- .qi_cetesb_temperatura(c(-6, -5), "delta", n = 2), "undefined")
  expect_true(is.na(q[1]))
  expect_false(is.na(q[2]))

  # Sem algum dos nove parametros o IQA e inviabilizado (p. 4)
  d <- d1()
  d$turbidez <- NA
  expect_true(is.na(iqa(d)$IQA))
})

test_that("Modos delta/reference avisam sobre a divergencia da curva q6", {
  expect_warning(iqa(d1(), temperature_method = "delta", delta_temperature = 2), "does not match")
  expect_silent(iqa(d1()))
})

test_that("Curvas corrigidas seguem a Figura 1 (leitura visual, tolerancia 8 pontos)", {
  # Pontos lidos da Figura 1 do Apendice D CETESB 2023. Sao leituras visuais
  # aproximadas; servem para rejeitar erros grosseiros como os da v0.10.0
  # (DBO 10 -> 73, NT 20 -> 72, pH 8.6 -> 0.0002), nao para validar coeficientes.
  expect_lte(abs(.qi_cetesb_dbo(10) - 30), 8)
  expect_lte(abs(.qi_cetesb_nt(20) - 32), 8)
  expect_lte(abs(.qi_cetesb_ph(8.6) - 72), 8)
})

# ------------------------------------------------------------------------------
# Gabaritos externos: valores de qi calculados por planilha independente em
# IBEAS (2015), VI Congresso Brasileiro de Gestao Ambiental, trabalho VIII-041,
# Tabelas 4, 8, 9, 10 e 11 (Tabela 4 = exemplo de von Sperling, 2007, p. 254).
# Os valores sao publicados com 2 casas; tolerancias: 0.06 (arredondamento) e
# 0.15 para OD (a fonte parte de %sat ja arredondada).
# ------------------------------------------------------------------------------
cs25 <- function() (14.62 - 0.3898 * 25 + 0.006969 * 25^2 - 0.00005896 * 25^3)
od_from_psat <- function(p) p / 100 * cs25()

test_that("Gabarito externo IBEAS/von Sperling: qi fora dos trechos em disputa", {
  expect_equal(.qi_cetesb_coliformes(10), 67.00, tolerance = 0.06)
  expect_equal(.qi_cetesb_coliformes(2), 90.10, tolerance = 0.06)
  expect_equal(.qi_cetesb_coliformes(160000), 3)
  expect_equal(.qi_cetesb_ph(6.8), 87.72, tolerance = 0.06)
  expect_equal(.qi_cetesb_ph(7.1), 92.00, tolerance = 0.06)
  expect_equal(.qi_cetesb_ph(7.3), 92.50, tolerance = 0.06)
  expect_equal(.qi_cetesb_dbo(2), 78.12, tolerance = 0.06)
  expect_equal(.qi_cetesb_dbo(3), 69.06, tolerance = 0.06)
  expect_equal(.qi_cetesb_dbo(20), 11.05, tolerance = 0.06)
  expect_equal(.qi_cetesb_nt(0.11), 99.10, tolerance = 0.06)
  expect_equal(.qi_cetesb_nt(0.14), 98.90, tolerance = 0.06)
  expect_equal(.qi_cetesb_nt(0.30), 97.58, tolerance = 0.06)
  expect_equal(.qi_cetesb_pt(0.01, "P"), 96.30, tolerance = 0.06)
  expect_equal(.qi_cetesb_pt(0.05, "P"), 86.03, tolerance = 0.06)
  expect_equal(.qi_cetesb_pt(0.44, "P"), 34.30, tolerance = 0.06)
  expect_equal(.qi_cetesb_turbidez(16.6), 66.30, tolerance = 0.06)
  expect_equal(.qi_cetesb_turbidez(20), 61.87, tolerance = 0.06)
  expect_equal(.qi_cetesb_solidos_totais(100), 85.47, tolerance = 0.06)
  expect_equal(.qi_cetesb_solidos_totais(282), 62.40, tolerance = 0.06)
  expect_lte(abs(.qi_cetesb_od(od_from_psat(82), 25, 0) - 87.60), 0.15)
  expect_lte(abs(.qi_cetesb_od(od_from_psat(66.1), 25, 0) - 69.60), 0.15)
})

test_that("Gabarito externo: exemplo de von Sperling (2007, p. 254) da IQA = 81", {
  q <- c(od = .qi_cetesb_od(od_from_psat(82), 25, 0),
         coliformes_termotolerantes = .qi_cetesb_coliformes(10),
         ph = .qi_cetesb_ph(6.8), dbo = .qi_cetesb_dbo(3), nt_total = .qi_cetesb_nt(0.3),
         p_total = .qi_cetesb_pt(0.05, "P"), temperatura = 94,
         turbidez = .qi_cetesb_turbidez(20), solidos_totais = .qi_cetesb_solidos_totais(100))
  w <- .iqa_cetesb_weights[names(q)]
  expect_equal(round(prod(q^w)), 81)

  d <- data.frame(od = od_from_psat(82), temperatura = 25, coliformes = 10, ph = 6.8,
                  dbo = 3, nt_total = 0.3, p_total = 0.05, turbidez = 20, solidos_totais = 100)
  expect_equal(round(iqa(d)$IQA), 81)
})

test_that("Divergencia documentada: planilha IBEAS usa log10 em DBO 5-15 (DBO 12 -> 70.6)", {
  # O pacote segue a curva oficial (ln): DBO 12 -> ~26. Este teste fixa a
  # divergencia para que uma troca de base nao passe despercebida.
  expect_equal(104.67 - 31.5463 * log10(12), 70.60, tolerance = 0.06)
  expect_equal(.qi_cetesb_dbo(12), 104.67 - 31.5463 * log(12))
  expect_lt(.qi_cetesb_dbo(12), 40)
})

test_that("Classe e atribuida ao IQA nao arredondado", {
  d <- data.frame(
    od = seq(3, 9, length.out = 40), temperatura = 25, dbo = seq(1, 12, length.out = 40),
    coliformes = round(10^seq(0.5, 4.5, length.out = 40)), ph = seq(6, 8.8, length.out = 40),
    nt_total = seq(0.2, 15, length.out = 40), p_total = seq(0.01, 0.6, length.out = 40),
    turbidez = seq(2, 90, length.out = 40), solidos_totais = seq(40, 450, length.out = 40)
  )
  out <- iqa(d, details = TRUE)
  cp <- attr(out, "components")
  expect_identical(out$IQA_status, classify_iqa(cp$IQA_unrounded))
})

test_that("Modos de temperatura delta/reference ficam marcados como experimentais", {
  o <- suppressWarnings(iqa(d1(), temperature_method = "delta", delta_temperature = 1))
  expect_true("experimental_temperature_curve" %in% attr(o, "iqa_nonstandard"))
  expect_false("experimental_temperature_curve" %in% attr(iqa(d1()), "iqa_nonstandard"))
})

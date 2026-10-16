# tests/testthat/test-iqa-igam-engine.R
# Motor IGAM experimental (.iqa_igam_compute): subindices do SCQA 2005,
# turbidez k = 1.5, conversoes e regras de ausentes/censura. Apenas dados
# SINTETICOS: os registros historicos do IGAM nao sao redistribuidos no pacote
# (validacao historica documentada em inst/methodology/IGAM_VALIDATION.md).

test_that("turbidez: k = 1.5 nos pontos de controle e salto em 100 UNT", {
  tu <- c(0, 20, 25, 30, 40, 50, 60, 80, 100)
  expect_equal(.qi_igam_turbidez(tu),
               c(100.003, 62.490, 57.022, 52.140, 43.905, 37.396, 32.196, 24.020, 16.859),
               tolerance = 1e-4)
  expect_equal(.qi_igam_turbidez(100 + 1e-9), 5)
  expect_equal(.qi_igam_turbidez(1000), 5)
  # Expressao impressa (k = 15) so para auditoria: nao monotonica
  expect_equal(.qi_igam_turbidez(20, k = 15), 51.132, tolerance = 1e-4)
  expect_gt(.qi_igam_turbidez(80, k = 15), .qi_igam_turbidez(40, k = 15))
  # k = 1.5 decrescente em [0, 100]
  expect_true(all(diff(.qi_igam_turbidez(seq(0, 100, 0.5))) < 0))
  expect_equal(.iqa_igam_spec$turbidity_k, 1.5)
})

test_that("subindices do SCQA 2005 nas fronteiras", {
  expect_equal(.igam_cs(25, 0, 0), 8.308, tolerance = 1e-3)
  expect_equal(.qi_igam_od(c(0, 60)), c(3.1, 60.9), tolerance = 1e-2)
  # Descontinuidade da propria fonte em 100%: trecho <= 100 da 99.6; o
  # polinomio do trecho > 100 da 100.02 (diagnostico; nao suavizado)
  expect_equal(.qi_igam_od(100), 99.6, tolerance = 1e-3)
  expect_equal(.qi_igam_od(100 + 1e-9), 100.02, tolerance = 1e-3)
  expect_equal(.qi_igam_od(140), 76.56, tolerance = 1e-3)
  expect_equal(.qi_igam_od(140.01), 47)
  expect_equal(.qi_igam_coliformes(1), 98.24034)
  expect_equal(.qi_igam_coliformes(1e5 + 1), 3)
  expect_true(is.na(.qi_igam_coliformes(0.5)))   # dominio nao definido
  expect_equal(.qi_igam_ph(c(2, 12, 12.01)), c(2, 2.19, 3), tolerance = 1e-2)
  expect_equal(.qi_igam_dbo(30), 4.98, tolerance = 1e-2)
  expect_equal(.qi_igam_dbo(30.01), 2)
  expect_equal(.qi_igam_nitrato(10), 49.17, tolerance = 1e-6)
  expect_equal(.qi_igam_nitrato(90.01), 1)
  expect_equal(.qi_igam_fosfato(10.01), 5)
  expect_equal(.qi_igam_solidos(c(0, 500)), c(80, 33.95), tolerance = 1e-3)
  expect_equal(.qi_igam_solidos(500.1), 30)
})

test_that("motor: amostra sintetica com k = 1.5 x k = 15 (controle de auditoria)", {
  # Amostra SINTETICA A1 de audit/igam_valores_esperados_sinteticos.py
  d <- data.frame(od = 7, temperatura = 25, cloreto = 10, e_coli = 100, ph = 7.3, dbo = 2,
                  nitrato = 1, p_total = 0.1, turbidez = 5, solidos_totais = 100)
  r <- .iqa_igam_compute(d, nitrate_unit = "N", phosphorus_input = "P", altitude_m = 0)
  expect_equal(r$IQA_raw, 75.479124819573599, tolerance = 1e-12)
  r15 <- .iqa_igam_compute(d, nitrate_unit = "N", phosphorus_input = "P", altitude_m = 0,
                           turbidity_k = 15)
  # Com k = 15 so o qi de turbidez muda: a razao dos IQAs e a razao dos qi^0,08
  expect_equal(r15$IQA_raw / r$IQA_raw,
               (.qi_igam_turbidez(5, k = 15) / .qi_igam_turbidez(5)) ^ 0.08, tolerance = 1e-12)
})

test_that("nitrato exige unidade explicita e e convertido de N para NO3", {
  d <- data.frame(od = 7, temperatura = 25, cloreto = 10, e_coli = 100, ph = 7, dbo = 2,
                  nitrato = 1, fosfato_total = 0.1, turbidez = 5, solidos_totais = 100)
  expect_error(.iqa_igam_compute(d), "nitrate_unit")
  rN <- .iqa_igam_compute(d, nitrate_unit = "N")
  rNO3 <- .iqa_igam_compute(d, nitrate_unit = "NO3")
  expect_equal(rN$nitrato_mgL_NO3, 62.004 / 14.007)
  expect_equal(rNO3$nitrato_mgL_NO3, 1)
  expect_equal(rN$qi_nitrato, .qi_igam_nitrato(62.004 / 14.007))
  expect_equal(attr(rN, "igam_config")$nitrate_unit, "N")
})

test_that("fosforo: PO4 direto; P so com phosphorus_input = 'P'; ortofosfato nunca", {
  base <- data.frame(od = 7, temperatura = 25, cloreto = 10, e_coli = 100, ph = 7, dbo = 2,
                     nitrato = 1, turbidez = 5, solidos_totais = 100)
  dp <- cbind(base, p_total = 0.1)
  r_po4_missing <- .iqa_igam_compute(dp, nitrate_unit = "N")
  expect_equal(unname(unlist(r_po4_missing["w_fosfato_total"])), 0)   # ausente: peso redistribuido
  r_p <- .iqa_igam_compute(dp, nitrate_unit = "N", phosphorus_input = "P")
  expect_equal(r_p$fosfato_total_mgL_PO4, 0.1 * 94.9714 / 30.9738)
  dortho <- cbind(base, p_ortofosfato = 0.1)
  expect_true(any(grepl("orthophosphate", .igam_check_inputs(dortho)$problems)))
  r_o <- .iqa_igam_compute(dortho, nitrate_unit = "N")
  expect_true(is.na(r_o$qi_fosfato_total))
  # nitrogenio total nao substitui nitrato
  dnt <- base; dnt$nitrato <- NULL; dnt$nt_total <- 2; dnt$fosfato_total <- 0.1
  expect_true(any(grepl("total nitrogen", .igam_check_inputs(dnt)$problems)))
  expect_true(is.na(.iqa_igam_compute(dnt)$qi_nitrato))
})

test_that("ausentes: OD/microbiologico obrigatorios; cloreto sem valor padrao; redistribuicao", {
  d <- data.frame(od = c(7, NA, 7, 7, 7), temperatura = 25, cloreto = c(10, 10, NA, 10, 10),
                  e_coli = c(100, 100, 100, 0.5, 100), ph = 7, dbo = c(2, 2, 2, 2, NA),
                  nitrato = 1, fosfato_total = 0.1, turbidez = 5, solidos_totais = 100)
  r <- .iqa_igam_compute(d, nitrate_unit = "NO3")
  expect_false(is.na(r$IQA_raw[1]))
  expect_true(is.na(r$IQA_raw[2])); expect_match(r$observacao[2], "od")
  expect_true(is.na(r$IQA_raw[3])); expect_match(r$observacao[3], "cloreto")
  expect_true(is.na(r$IQA_raw[4])); expect_match(r$observacao[4], "< 1 NMP")
  # DBO ausente: 0.10 repartido igualmente entre os 8 restantes
  w5 <- unlist(r[5, grep("^w_", names(r))])
  expect_equal(sum(w5), 1)
  expect_equal(unname(w5["w_dbo"]), 0)
  expect_equal(unname(w5["w_od"]), 0.17 + 0.10 / 8)
  expect_match(r$observacao[5], "dbo")
  # Classificacao IGAM vinculada ao motor
  expect_equal(as.character(r$IQA_status[1]), as.character(classify_iqa(r$IQA[1], "igam")))
  expect_equal(attr(r, "iqa_classification"), "igam")
})

test_that("valores invalidos e censurados sao explicitos", {
  d <- data.frame(od = "7", temperatura = 25, cloreto = "10", e_coli = ">24196", ph = 7,
                  dbo = "<2", nitrato = "<0.02", fosfato_total = "0.1", turbidez = c("5", "-1"),
                  solidos_totais = "100")
  expect_warning(r <- .iqa_igam_compute(d, nitrate_unit = "N"), "turbidez")
  expect_true(is.na(r$qi_turbidez[2]))            # negativo: NA, nao truncado
  expect_equal(unname(unlist(r[2, "w_turbidez"])), 0)
  cz <- attr(r, "igam_censored")
  expect_true(all(cz$microbiologico)); expect_true(all(cz$dbo)); expect_false(any(cz$turbidez))
  expect_equal(attr(r, "igam_config")$censor_policy, "limit")
  r_half <- suppressWarnings(.iqa_igam_compute(d, nitrate_unit = "N", censor_policy = "half_limit"))
  expect_false(isTRUE(all.equal(r$qi_dbo, r_half$qi_dbo)))
  expect_equal(nrow(.iqa_igam_compute(d[0, ], nitrate_unit = "N")), 0L)
})

test_that("temperatura: qs constante 92 (Projeto Aguas de Minas)", {
  d <- data.frame(od = 7, temperatura = 25, cloreto = 10, e_coli = 100, ph = 7, dbo = 2,
                  nitrato = 1, fosfato_total = 0.1, turbidez = 5, solidos_totais = 100)
  r <- .iqa_igam_compute(d, nitrate_unit = "NO3")
  expect_equal(r$qi_temperatura, 92)
  expect_equal(.iqa_igam_spec$temperature_qs, 92)
  expect_equal(.iqa_igam_spec$temperature_qs_scqa_2005, 93)
})


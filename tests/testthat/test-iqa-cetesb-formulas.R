# tests/testthat/test-iqa-cetesb-formulas.R
# Testes unitarios rigorosos das equacoes analiticas CETESB (Q1 a Q9) e limites de fronteira

test_that("Qi de Oxigenio Dissolvido calcula saturacao e faixas corretamente", {
  # Cs a 25 C e 0 m de altitude: ~8.263 mg/L
  # OD = 8.263 -> psat ~ 100% -> Qi = 100
  od_100 <- .qi_cetesb_od(8.263, temp = 25, altitude_m = 0)
  expect_true(od_100 >= 98 && od_100 <= 100)

  # psat = 0% -> Qi = 3
  od_0 <- .qi_cetesb_od(0, temp = 25, altitude_m = 0)
  expect_equal(od_0, 3, tolerance = 0.01)

  # psat > 140% -> Qi = 47
  od_supersat <- .qi_cetesb_od(15, temp = 25, altitude_m = 0)
  expect_equal(od_supersat, 47)

  # Fronteiras de psat: 50%, 85%, 100%, 140%
  q_50 <- .qi_cetesb_od(8.263 * 0.50, temp = 25, altitude_m = 0)
  expect_true(is.finite(q_50) && q_50 > 0 && q_50 <= 100)

  q_85 <- .qi_cetesb_od(8.263 * 0.85, temp = 25, altitude_m = 0)
  expect_true(is.finite(q_85) && q_85 > 0 && q_85 <= 100)
})

test_that("Qi de Coliformes e E. coli aplica fator 1.25x e intervalos corretos", {
  # col <= 1 -> Qi = 100
  expect_equal(.qi_cetesb_coliformes(1, "thermotolerant_coliforms"), 100)
  expect_equal(.qi_cetesb_coliformes(0, "thermotolerant_coliforms"), 100)

  # col = 10 (logC = 1) -> Qi = 100 - 33*1 = 67
  expect_equal(.qi_cetesb_coliformes(10, "thermotolerant_coliforms"), 67, tolerance = 0.01)

  # col = 100000 (logC = 5) -> Qi = 100 - 37.2*5 + 3.60743*25 = 4.18575
  expect_equal(.qi_cetesb_coliformes(100000, "thermotolerant_coliforms"), 4.19, tolerance = 0.02)

  # col > 100000 -> Qi = 3
  expect_equal(.qi_cetesb_coliformes(200000, "thermotolerant_coliforms"), 3)

  # E. coli com fator 1.25: 8 NMP de E. coli = 10 NMP coliformes -> Qi = 67
  expect_equal(.qi_cetesb_coliformes(8, "e_coli"), 67, tolerance = 0.01)
})

test_that("Qi de pH respeita os intervalos analiticos oficiais", {
  # pH <= 2 -> 2
  expect_equal(.qi_cetesb_ph(2.0), 2)
  expect_equal(.qi_cetesb_ph(1.0), 2)

  # pH = 7.0 (neutro ideal) -> Qi proximo de 90-93
  q_ph7 <- .qi_cetesb_ph(7.0)
  expect_true(q_ph7 >= 88 && q_ph7 <= 95)

  # pH = 8.5 -> 216 - 16*8.5 = 80
  expect_equal(.qi_cetesb_ph(8.5), 80, tolerance = 0.1)

  # pH > 12 -> 3
  expect_equal(.qi_cetesb_ph(13.0), 3)
})

test_that("Qi de DBO5 utiliza base exponencial natural e 99.96", {
  # DBO = 0 -> 99.96 * exp(0) = 99.96 -> ~100
  expect_equal(.qi_cetesb_dbo(0), 99.96, tolerance = 0.01)

  # DBO = 2 -> 99.96 * exp(-0.1232728 * 2) = 78.11
  expect_equal(.qi_cetesb_dbo(2), 99.96 * exp(-0.1232728 * 2), tolerance = 0.01)

  # DBO = 5 -> 99.96 * exp(-0.1232728 * 5) = 54.01
  expect_equal(.qi_cetesb_dbo(5), 99.96 * exp(-0.1232728 * 5), tolerance = 0.01)

  # DBO > 30 -> 2
  expect_equal(.qi_cetesb_dbo(35), 2)
})

test_that("Qi de Nitrogenio Total utiliza as faixas oficiais e base exp", {
  # NT = 0 -> 100
  expect_equal(.qi_cetesb_nt(0), 100, tolerance = 0.01)

  # NT = 10 -> 100 - 8.169*10 + 0.3059*100 = 48.90
  expect_equal(.qi_cetesb_nt(10), 48.90, tolerance = 0.01)

  # NT > 100 -> 1
  expect_equal(.qi_cetesb_nt(150), 1)
})

test_that("Qi de Fosforo Total converte P para PO4 (fator 3.066) e calcula corretamente", {
  # PT = 0 -> 99.0 * exp(0) = 99.0
  expect_equal(.qi_cetesb_pt(0, "P"), 99.0, tolerance = 0.01)

  # PT = 0.05 mg/L P -> PO4 = 0.05 * 3.066 = 0.1533 mg/L -> 99 * exp(-0.91629 * 0.1533) = 85.99
  expected_po4 <- 0.05 * 3.066
  expect_equal(.qi_cetesb_pt(0.05, "P"), 99.0 * exp(-0.91629 * expected_po4), tolerance = 0.01)

  # Se ja for PO4 (basis = "PO4")
  expect_equal(.qi_cetesb_pt(expected_po4, "PO4"), 99.0 * exp(-0.91629 * expected_po4), tolerance = 0.01)

  # PT > 10 mg/L PO4 -> 5
  expect_equal(.qi_cetesb_pt(15, "PO4"), 5)
})

test_that("Qi de Turbidez usa faixas 0-25, 25-100 e >100", {
  # Turb = 0 -> 100.17 -> 100
  expect_equal(.qi_cetesb_turbidez(0), 100, tolerance = 0.2)

  # Turb = 25 -> 100.17 - 2.67*25 + 0.03775*(25^2) = 57.01
  expect_equal(.qi_cetesb_turbidez(25), 57.01, tolerance = 0.02)

  # Turb = 50 -> 84.76 * exp(-0.016206 * 50) = 37.69
  expect_equal(.qi_cetesb_turbidez(50), 84.76 * exp(-0.016206 * 50), tolerance = 0.01)

  # Turb > 100 -> 5
  expect_equal(.qi_cetesb_turbidez(150), 5)
})

test_that("Qi de Solidos Totais usa faixas oficiais (e nao TDS)", {
  # ST = 0 -> 79.75
  expect_equal(.qi_cetesb_solidos_totais(0), 79.75, tolerance = 0.01)

  # ST = 100 -> 79.75 + 0.166*100 - 0.001088*10000 = 85.47
  expect_equal(.qi_cetesb_solidos_totais(100), 85.47, tolerance = 0.01)

  # ST = 150 -> 79.75 + 0.166*150 - 0.001088*22500 = 80.17
  expect_equal(.qi_cetesb_solidos_totais(150), 80.17, tolerance = 0.01)

  # ST = 500 -> 101.67 - 0.13917*500 = 32.085
  expect_equal(.qi_cetesb_solidos_totais(500), 32.085, tolerance = 0.01)

  # ST > 500 -> 32
  expect_equal(.qi_cetesb_solidos_totais(600), 32)
})

# tests/testthat/test-iqa-temperature.R
# Testes especificos de temperatura e separacao entre temperatura da agua e Delta T

test_that("Caso A - Sonda informa apenas temperatura da agua (sem Delta T): Qi_temperatura = 94", {
  d <- data.frame(
    od = 6.5, temperatura = 29, dbo = 2, coliformes = 200,
    ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8,
    solidos_totais = 120
  )
  comps <- iqa_components(d)
  expect_equal(comps$qi_temp, 94)
})

test_that("Caso B - Variacao apenas da temperatura da agua altera Qi_OD mas preserva Qi_temperatura = 94", {
  d25 <- data.frame(
    od = 6.5, temperatura = 25, dbo = 2, coliformes = 200,
    ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8,
    solidos_totais = 120
  )
  d30 <- data.frame(
    od = 6.5, temperatura = 30, dbo = 2, coliformes = 200,
    ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8,
    solidos_totais = 120
  )
  c25 <- iqa_components(d25)
  c30 <- iqa_components(d30)

  # Qi de temperatura permanece 94
  expect_equal(c25$qi_temp, 94)
  expect_equal(c30$qi_temp, 94)

  # Qi de OD se altera devido a concentracao de saturacao Cs(T)
  expect_false(c25$qi_od == c30$qi_od)
})

test_that("Caso C - Delta T fornecido explicitamente calcula Qi pela curva termica", {
  d <- data.frame(
    od = 6.5, temperatura = 25, delta_temperatura = 2, dbo = 2, coliformes = 200,
    ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8,
    solidos_totais = 120
  )
  comps <- iqa_components(d, temperature_method = "delta")
  # 0 < dT <= 5: 100 - 3.8*2 + 0.1*(2^2) = 100 - 7.6 + 0.4 = 92.8
  expect_equal(comps$qi_temp, 92.8, tolerance = 0.01)
})

test_that("Caso D - Referencia termica fornecida calcula Delta T = T_agua - T_ref", {
  d <- data.frame(
    od = 6.5, temperatura = 29, dbo = 2, coliformes = 200,
    ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8,
    solidos_totais = 120
  )
  comps <- iqa_components(d, temperature_method = "reference", temperature_reference = 27)
  # Delta T = 29 - 27 = 2 -> Qi = 92.8
  expect_equal(comps$qi_temp, 92.8, tolerance = 0.01)
})

test_that("Caso E - Impedir inferencia incorreta (temperatura_agua = 29 jamais deve ser tratada como Delta T = 29)", {
  d <- data.frame(
    od = 6.5, temperatura = 29, dbo = 2, coliformes = 200,
    ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8,
    solidos_totais = 120
  )
  comps <- iqa_components(d)
  # Se fosse tratado como Delta T = 29, Qi seria 9 (extremo degradado)
  expect_false(comps$qi_temp == 9)
  expect_equal(comps$qi_temp, 94)
})

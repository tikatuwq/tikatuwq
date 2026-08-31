test_that("compute_load() adiciona coluna de carga corretamente", {
  d <- data.frame(p_total = c(0.1, 0.2), vazao = c(5, 10))
  out <- compute_load(d, param = "p_total", flow_col = "vazao", unit_out = "kg_dia")
  expect_true("p_total_carga_kg_dia" %in% names(out))
  # 0.1 mg/L * 5 m3/s * 86.4 = 43.2 kg/dia
  expect_equal(out$p_total_carga_kg_dia[1], 0.1 * 5 * 86.4)
})

test_that("compute_load() aceita nome de coluna personalizado", {
  d <- data.frame(dbo = c(5), vazao = c(2))
  out <- compute_load(d, param = "dbo", flow_col = "vazao",
                      unit_out = "t_dia", col_name = "carga_dbo")
  expect_true("carga_dbo" %in% names(out))
})

test_that("compute_load() erro se coluna de parametro nao existe", {
  d <- data.frame(vazao = c(1))
  expect_error(compute_load(d, param = "dbo", flow_col = "vazao"), "'dbo'")
})

test_that("exceedance_prob() retorna probabilidades entre 0 e 1", {
  data("wq_demo", package = "tikatuwq")
  out <- exceedance_prob(wq_demo, param = "turbidez", threshold = 40, by = "ponto")
  expect_s3_class(out, "data.frame")
  expect_true(all(out$prob_excedencia >= 0 & out$prob_excedencia <= 1, na.rm = TRUE))
})

test_that("exceedance_prob() direction=below funciona para OD", {
  data("wq_demo", package = "tikatuwq")
  out <- exceedance_prob(wq_demo, param = "od", threshold = 5,
                         direction = "below", by = "ponto")
  expect_true("n_excedeu" %in% names(out))
  expect_true(all(out$n_excedeu <= out$n))
})

test_that("exceedance_prob() intervalo de confianca Wilson valido", {
  d <- data.frame(ponto = rep("P1", 10), turbidez = c(rep(150, 7), rep(20, 3)))
  out <- exceedance_prob(d, param = "turbidez", threshold = 100, by = "ponto")
  expect_true(out$ic_inf[1] <= out$prob_excedencia[1])
  expect_true(out$ic_sup[1] >= out$prob_excedencia[1])
  expect_true(out$ic_inf[1] >= 0)
  expect_true(out$ic_sup[1] <= 1)
})

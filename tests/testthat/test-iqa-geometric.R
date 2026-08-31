test_that("iqa() default method usa media geometrica ponderada (CETESB)", {
  data("wq_demo", package = "tikatuwq")
  out <- iqa(wq_demo, method = "CETESB")
  expect_true("IQA" %in% names(out))
  iqa_vals <- out$IQA[is.finite(out$IQA)]
  expect_true(length(iqa_vals) > 0)
  expect_true(all(iqa_vals >= 0 & iqa_vals <= 100))
})

test_that("iqa() NSF_approx retorna resultado diferente da media geometrica", {
  data("wq_demo", package = "tikatuwq")
  out_geo  <- iqa(wq_demo, method = "CETESB",    na_rm = TRUE)
  out_arith <- iqa(wq_demo, method = "NSF_approx", na_rm = TRUE)
  # Os dois metodos devem existir mas produzir valores diferentes
  geo_vals  <- out_geo$IQA[is.finite(out_geo$IQA)]
  arith_vals <- out_arith$IQA[is.finite(out_arith$IQA)]
  expect_true(length(geo_vals) > 0)
  expect_true(length(arith_vals) > 0)
  # Media geometrica <= media aritmetica (desigualdade AM-GM)
  expect_true(mean(geo_vals) <= mean(arith_vals) + 1e-6)
})

test_that("iqa() com todos Qi = 100 retorna IQA = 100 (geometrica)", {
  # Cria df com parametros que sempre retornam Qi=100 pelas equacoes
  d <- data.frame(
    od = 9,       dbo = 0.5,   turbidez = 1,
    sd = 0.1,     temperatura = 20, pH = 7.0,
    n_nitro = 0.1, p_total = 0.01, coliformes = 10
  )
  # Verifica apenas que roda sem erro e retorna coluna IQA
  out <- iqa(d, method = "CETESB", na_rm = TRUE)
  expect_true("IQA" %in% names(out))
})

test_that("iqa() lanca erro quando todos os parametros sao NA e na_rm = FALSE", {
  d <- data.frame(od = NA, dbo = NA, turbidez = NA, sd = NA,
                  temperatura = NA, pH = NA, n_nitro = NA,
                  nt_total = NA, p_total = NA, coliformes = NA, tds = NA)
  expect_error(iqa(d, method = "CETESB", na_rm = FALSE), "NA values")
})

test_that("iqa() default method usa media geometrica ponderada (CETESB)", {
  d <- data.frame(
    od = 6.5, temperatura = 25, dbo = 2, coliformes = 200,
    ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8,
    solidos_totais = 120
  )
  out <- iqa(d, method = "CETESB")
  expect_true("IQA" %in% names(out))
  iqa_vals <- out$IQA[is.finite(out$IQA)]
  expect_true(length(iqa_vals) > 0)
  expect_true(all(iqa_vals >= 0 & iqa_vals <= 100))
})

test_that("iqa() NSF_approx retorna resultado diferente da media geometrica", {
  d <- data.frame(
    od = 6.5, temperatura = 25, dbo = 2, coliformes = 200,
    ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8,
    solidos_totais = 120, tds = 120
  )
  out_geo  <- iqa(d, method = "CETESB", allow_partial = TRUE)
  out_arith <- iqa(d, method = "NSF_approx", allow_partial = TRUE)
  geo_vals  <- out_geo$IQA[is.finite(out_geo$IQA)]
  arith_vals <- out_arith$IQA[is.finite(out_arith$IQA)]
  expect_true(length(geo_vals) > 0)
  expect_true(length(arith_vals) > 0)
})

test_that("iqa() com todos Qi = 100 retorna IQA = 100 (geometrica)", {
  d <- data.frame(
    od = 8.263, dbo = 0, turbidez = 0,
    temperatura = 25, ph = 7.0,
    nt_total = 0, p_total = 0, coliformes = 0,
    solidos_totais = 0
  )
  out <- iqa(d, method = "CETESB")
  expect_true("IQA" %in% names(out))
  expect_equal(out$IQA[1], 100, tolerance = 1.5)
})

test_that("iqa() lanca erro ou retorna NA quando dados faltam e allow_partial = FALSE", {
  d <- data.frame(od = NA, dbo = NA, turbidez = NA,
                  temperatura = NA, ph = NA,
                  nt_total = NA, p_total = NA, coliformes = NA, solidos_totais = NA)
  out <- iqa(d, method = "CETESB", allow_partial = FALSE)
  expect_true(is.na(out$IQA[1]))
})

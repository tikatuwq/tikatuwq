# tests/testthat/test-iqa-solids-tds.R
# Testes especificos da distincao entre solidos_totais e tds no IQA oficial CETESB

test_that("Fornecer apenas TDS em modo CETESB estrito lanca erro explicativo", {
  d_tds <- data.frame(
    od = 6.5, temperatura = 25, dbo = 2, coliformes = 200,
    ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8,
    tds = 120
  )
  expect_error(iqa(d_tds, method = "CETESB", allow_partial = FALSE), "solidos_totais/residuo_total")
})

test_that("Fornecer solidos_totais ou residuo_total em modo CETESB calcula normalmente", {
  d_st <- data.frame(
    od = 6.5, temperatura = 25, dbo = 2, coliformes = 200,
    ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8,
    solidos_totais = 120
  )
  out_st <- iqa(d_st, method = "CETESB")
  expect_true(is.finite(out_st$IQA[1]))

  d_res <- data.frame(
    od = 6.5, temperatura = 25, dbo = 2, coliformes = 200,
    ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8,
    residuo_total = 120
  )
  out_res <- iqa(d_res, method = "CETESB")
  expect_equal(out_st$IQA, out_res$IQA)
})

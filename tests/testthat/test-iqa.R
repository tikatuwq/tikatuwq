test_that("iqa retorna coluna IQA", {
  df <- data.frame(pH=7.2, turbidez=4, od=7, dbo=2, nt_total=0.8,
                   p_total=0.05, tds=300, temperatura=24, coliformes=150)
  out <- iqa(df)
  expect_true("IQA" %in% names(out))
  expect_true(out$IQA[1] > 0 && out$IQA[1] <= 100)
})

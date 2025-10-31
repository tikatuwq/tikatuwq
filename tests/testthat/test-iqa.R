test_that("iqa retorna coluna IQA", {
  df <- data.frame(pH=7.2, turbidez=4, od=7, dbo=2, nt_total=0.8,
                   p_total=0.05, tds=300, temperatura=24, coliformes=150)
  out <- iqa(df)
  expect_true("IQA" %in% names(out))
  expect_true(out$IQA[1] > 0 && out$IQA[1] <= 100)
})

test_that("iqa handles missing columns with na_rm", {
  d <- data.frame(od=8, coliformes=200, dbo=3, nt_total=1, p_total=0.05,
                  turbidez=10, tds=150, ph=7.2, temperatura=24)
  d2 <- subset(d, select=-tds)
  expect_error(iqa(d2, na_rm = FALSE))
  out <- iqa(d2, na_rm = TRUE)
  expect_true("IQA" %in% names(out))
})

test_that("iqa retorna coluna IQA", {
  df <- data.frame(pH=7.2, turbidez=4, od=7, dbo=2, nt_total=0.8,
                   p_total=0.05, solidos_totais=300, temperatura=24, coliformes=150)
  out <- iqa(df)
  expect_true("IQA" %in% names(out))
  expect_true(out$IQA[1] > 0 && out$IQA[1] <= 100)
})

test_that("iqa handles missing columns with allow_partial", {
  d <- data.frame(od=8, coliformes=200, dbo=3, nt_total=1, p_total=0.05,
                  turbidez=10, solidos_totais=150, ph=7.2, temperatura=24)
  d2 <- subset(d, select=-solidos_totais)
  out_na <- iqa(d2, allow_partial = FALSE)
  expect_true(is.na(out_na$IQA[1]))
  out_partial <- iqa(d2, allow_partial = TRUE)
  expect_true("IQA" %in% names(out_partial))
  expect_true(is.finite(out_partial$IQA[1]))
})

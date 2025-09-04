test_that("qi interpolation returns finite values", {
  tbl <- iqa_curve_table()[["od"]]
  vals <- qi_interp(c(4,5,6,8), tbl)
  expect_true(all(is.finite(vals)))
  expect_true(all(vals >= 0 & vals <= 100))
})

test_that("iqa curve-based works", {
  df <- data.frame(pH=7.2, turbidez=4, od=7, dbo=2, nt_total=0.8,
                   p_total=0.05, tds=300, temperatura=2, coliformes=150)
  out <- iqa(df, na_rm = TRUE)
  expect_true("IQA" %in% names(out))
  expect_true(out$IQA[1] > 0 && out$IQA[1] <= 100)
})

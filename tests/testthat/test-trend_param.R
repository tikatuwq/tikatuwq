test_that("trend_param calcula inclinacao e classifica tendencia", {
  set.seed(1)
  df <- data.frame(
    data = as.Date("2024-01-01") + 0:11*30,
    rio = "Demo", ponto = "P1",
    turbidez = 20 + (-0.5)*(0:11) + rnorm(12, 0, 0.5)
  )
  res <- trend_param(df, param = "turbidez", min_n = 6)
  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) == 1)
  expect_true(is.finite(res$slope_per_year))
  expect_true(res$trend %in% c("aumento","queda","estavel","indefinido"))
})

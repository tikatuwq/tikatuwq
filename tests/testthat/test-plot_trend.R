test_that("plot_trend retorna ggplot com linhas de tendencia", {
  skip_on_cran()  # evita flutuar por ruido aleatorio em CI
  set.seed(1)

  df <- data.frame(
    data    = as.Date("2024-01-01") + 0:11 * 30,
    rio     = rep("R1", 12),
    ponto   = "P1",
    turbidez = 20 + (-0.4) * (0:11) + rnorm(12, 0, 0.6)
  )

  g1 <- plot_trend(df, param = "turbidez", method = "theilsen")
  g2 <- plot_trend(df, param = "turbidez", method = "ols")
  g3 <- plot_trend(df, param = "turbidez", method = "loess")

  expect_s3_class(g1, "ggplot")
  expect_s3_class(g2, "ggplot")
  expect_s3_class(g3, "ggplot")

  expect_gte(length(g1$layers), 2)
  expect_gte(length(g2$layers), 2)
  expect_gte(length(g3$layers), 2)
})

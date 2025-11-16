test_that("plot_iqa returns ggplot object", {
  df <- data.frame(
    ponto = c("P1", "P2", "P3"),
    IQA = c(75, 65, 80)
  )
  
  p <- plot_iqa(df)
  
  expect_s3_class(p, "ggplot")
  expect_error(print(p), NA)  # Nao deve quebrar ao plotar
})

test_that("plot_iqa errors when IQA column missing", {
  df <- data.frame(ponto = c("P1", "P2"))
  
  expect_error(plot_iqa(df), "IQA")
})

test_that("plot_series returns ggplot object", {
  df <- data.frame(
    data = as.Date(c("2025-01-01", "2025-01-02", "2025-01-03")),
    turbidez = c(10, 12, 8),
    ponto = c("P1", "P1", "P1")
  )
  
  p <- plot_series(df, "turbidez")
  expect_s3_class(p, "ggplot")
  
  p2 <- plot_series(df, "turbidez", facet = "ponto")
  expect_s3_class(p2, "ggplot")
})

test_that("plot_series errors when required columns missing", {
  df <- data.frame(turbidez = c(10, 12))
  
  expect_error(plot_series(df, "turbidez"), "data")
  
  df2 <- data.frame(data = as.Date("2025-01-01"))
  expect_error(plot_series(df2, "turbidez"), "not found")
})

test_that("plot_box returns ggplot object", {
  df <- data.frame(
    ponto = c("P1", "P1", "P2", "P2"),
    turbidez = c(10, 12, 8, 9)
  )
  
  p <- plot_box(df, "turbidez", by = "ponto")
  expect_s3_class(p, "ggplot")
})

test_that("plot_box errors when columns missing", {
  df <- data.frame(ponto = c("P1", "P2"))
  
  expect_error(plot_box(df, "turbidez"), "turbidez")
  expect_error(plot_box(df, "turbidez", by = "ponto"), "turbidez")
})

test_that("plot_heatmap returns ggplot object", {
  df_long <- data.frame(
    data = as.Date(c("2025-01-01", "2025-01-02", "2025-01-01", "2025-01-02")),
    parametro = c("turbidez", "turbidez", "od", "od"),
    valor = c(10, 12, 8, 9)
  )
  
  p <- plot_heatmap(df_long)
  expect_s3_class(p, "ggplot")
})


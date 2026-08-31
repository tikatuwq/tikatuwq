test_that("wq_pca() retorna lista com elementos esperados", {
  skip_if_not_installed("ggplot2")
  data("wq_demo", package = "tikatuwq")
  params <- c("turbidez", "od", "dbo", "pH")
  params <- intersect(params, names(wq_demo))
  if (length(params) < 2) skip("Parametros insuficientes em wq_demo.")
  res <- wq_pca(wq_demo, params = params)
  expect_named(res, c("pca", "scores", "loadings", "variance"), ignore.order = TRUE)
  expect_s3_class(res$pca, "prcomp")
  expect_s3_class(res$scores, "data.frame")
})

test_that("wq_pca() retorna atributos de graficos", {
  skip_if_not_installed("ggplot2")
  data("wq_demo", package = "tikatuwq")
  params <- intersect(c("turbidez", "od", "dbo", "pH"), names(wq_demo))
  if (length(params) < 2) skip("Parametros insuficientes.")
  res <- wq_pca(wq_demo, params = params)
  expect_s3_class(attr(res, "biplot"),        "ggplot")
  expect_s3_class(attr(res, "screeplot"),     "ggplot")
  expect_s3_class(attr(res, "loadings_plot"), "ggplot")
})

test_that("wq_pca() variancia acumulada atinge 100%", {
  skip_if_not_installed("ggplot2")
  data("wq_demo", package = "tikatuwq")
  params <- intersect(c("turbidez", "od", "dbo", "pH"), names(wq_demo))
  if (length(params) < 2) skip("Parametros insuficientes.")
  res <- wq_pca(wq_demo, params = params)
  last_acc <- res$variance$acumulada_pct[nrow(res$variance)]
  expect_true(abs(last_acc - 100) < 0.1)
})

test_that("wq_pca() erro com menos de 2 parametros numericos", {
  d <- data.frame(ponto = c("P1", "P2"), od = c(7, 5))
  expect_error(wq_pca(d, params = "od"), "ao menos 2")
})

test_that("wq_pca() color_by aparece nos scores", {
  skip_if_not_installed("ggplot2")
  data("wq_demo", package = "tikatuwq")
  params <- intersect(c("turbidez", "od", "dbo"), names(wq_demo))
  if (length(params) < 2) skip("Parametros insuficientes.")
  res <- wq_pca(wq_demo, params = params, color_by = "ponto")
  expect_true("ponto" %in% names(res$scores))
})

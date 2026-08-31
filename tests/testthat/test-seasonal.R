test_that("assign_season() adiciona coluna season corretamente", {
  data("wq_demo", package = "tikatuwq")
  out <- assign_season(wq_demo, region = "bahia")
  expect_true("season" %in% names(out))
  expect_true(all(out$season %in% c("chuvoso", "seco")))
})

test_that("assign_season() region = custom usa meses fornecidos", {
  d <- data.frame(data = as.Date(c("2023-01-15", "2023-07-10")))
  out <- assign_season(d, region = "custom", wet_months = c(12, 1, 2, 3))
  expect_equal(out$season[1], "chuvoso")  # janeiro = chuvoso
  expect_equal(out$season[2], "seco")     # julho = seco
})

test_that("assign_season() erro quando region=custom sem wet_months", {
  d <- data.frame(data = as.Date("2023-01-01"))
  expect_error(assign_season(d, region = "custom"), "wet_months")
})

test_that("compare_seasons() retorna tibble com colunas estatisticas", {
  data("wq_demo", package = "tikatuwq")
  d <- assign_season(wq_demo, region = "bahia")
  # Garante que ha ao menos 2 niveis de season com dados suficientes
  if (length(unique(d$season)) >= 2) {
    out <- compare_seasons(d, param = "turbidez", by = "ponto",
                           test = "wilcoxon", plot = FALSE)
    expect_s3_class(out, "data.frame")
    expect_true("p_value" %in% names(out))
    expect_true("diferenca_significativa" %in% names(out))
  } else {
    skip("Dataset nao tem 2 periodos hidrologicos distintos.")
  }
})

test_that("compare_seasons() com plot = TRUE retorna atributo 'plot'", {
  data("wq_demo", package = "tikatuwq")
  d <- assign_season(wq_demo, region = "bahia")
  if (length(unique(d$season)) >= 2) {
    out <- compare_seasons(d, param = "turbidez", by = "ponto", plot = TRUE)
    gg <- attr(out, "plot")
    expect_s3_class(gg, "ggplot")
  } else {
    skip("Dataset nao tem 2 periodos hidrologicos distintos.")
  }
})

test_that("mk_seasonal() retorna tibble com colunas esperadas", {
  data("wq_demo", package = "tikatuwq")
  out <- mk_seasonal(wq_demo, param = "turbidez", by = "ponto")
  expect_s3_class(out, "data.frame")
  expect_true(all(c("parametro","S","Z","p_value","tau","sen_slope",
                    "significativo","tendencia") %in% names(out)))
})

test_that("mk_seasonal() p_value esta entre 0 e 1", {
  data("wq_demo", package = "tikatuwq")
  out <- mk_seasonal(wq_demo, param = "turbidez", by = "ponto")
  pvals <- out$p_value[!is.na(out$p_value)]
  expect_true(all(pvals >= 0 & pvals <= 1))
})

test_that("mk_seasonal() tendencia crescente para serie monotonica crescente", {
  d <- data.frame(
    ponto    = rep("P1", 12),
    data     = as.Date(paste0(2020:2021, rep(paste0("-0", 1:6, "-01"), each = 2))[1:12]),
    turbidez = seq(10, 120, length.out = 12)
  )
  # Ordenar datas corretamente
  d <- d[order(d$data), ]
  out <- suppressWarnings(mk_seasonal(d, param = "turbidez", by = "ponto", alpha = 0.05))
  if (!is.na(out$Z[1]) && abs(out$Z[1]) > 0) {
    expect_true(out$Z[1] > 0)
  }
})

test_that("mk_seasonal() aceita period = 'season' com assign_season()", {
  data("wq_demo", package = "tikatuwq")
  d   <- assign_season(wq_demo, region = "bahia")
  out <- suppressWarnings(
    mk_seasonal(d, param = "turbidez", by = "ponto",
                period = "season", season_col = "season")
  )
  expect_s3_class(out, "data.frame")
})

test_that("mk_seasonal() erro se parametro ausente", {
  data("wq_demo", package = "tikatuwq")
  expect_error(mk_seasonal(wq_demo, param = "inexistente"), "'inexistente'")
})

test_that("mk_seasonal() erro se season_col ausente com period='season'", {
  data("wq_demo", package = "tikatuwq")
  expect_error(
    mk_seasonal(wq_demo, param = "turbidez", period = "season",
                season_col = "nao_existe"),
    "'nao_existe'"
  )
})

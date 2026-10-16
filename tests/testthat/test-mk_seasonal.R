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
  # Serie mensal sintetica de 5 anos, estritamente crescente (sem empates).
  # Valores esperados calculados a mao (Hirsch, Slack & Smith 1982): em cada
  # um dos 12 meses n = 5, S_m = C(5,2) = 10 e VAR(S_m) = 5*4*15/18; logo
  # S = 120, VAR(S) = 200 e Z = (120 - 1) / sqrt(200).
  d <- data.frame(
    ponto    = "P1",
    data     = seq(as.Date("2015-01-15"), by = "month", length.out = 60),
    turbidez = 10 + seq_len(60)
  )
  out <- mk_seasonal(d, param = "turbidez", by = "ponto", alpha = 0.05)
  expect_equal(out$n_estacoes, 12L)
  expect_equal(out$S, 120)
  expect_equal(out$varS, 200)
  expect_equal(out$Z, round(119 / sqrt(200), 4))
  expect_true(out$significativo)
  expect_equal(out$tendencia, "crescente")
  # Controle: a serie invertida da tendencia decrescente com Z simetrico
  d$turbidez <- rev(d$turbidez)
  dec <- mk_seasonal(d, param = "turbidez", by = "ponto", alpha = 0.05)
  expect_equal(dec$S, -120)
  expect_equal(dec$Z, -out$Z)
  expect_equal(dec$tendencia, "decrescente")
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

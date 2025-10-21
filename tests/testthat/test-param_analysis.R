test_that("param_summary() calcula resumo e respeita filtros por ponto/rio", {
  skip_on_cran()  # mantem rápido e robusto no CRAN

  library(tikatuwq)
  data("wq_demo", package = "tikatuwq")

  # garante que temos as colunas esperadas para o teste
  expect_true("ponto" %in% names(wq_demo))
  # 'rio' pode não existir em alguns datasets; o teste só usa 'ponto'

  # 1) resumo para um ponto especifico
  pto <- unique(wq_demo$ponto)[1]
  res <- param_summary(wq_demo, parametro = "turbidez", pontos = pto)

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("n","mean","sd","min","median","max") %in% names(res)))

  # se a coluna ponto existir no output, deve ser sempre o ponto filtrado
  if ("ponto" %in% names(res)) {
    expect_true(all(res$ponto == pto))
  }

  # n deve ser a contagem de valores nao-NA do parametro, após filtro
  n_expected <- sum(!is.na(wq_demo$turbidez[wq_demo$ponto == pto]))
  # quando ha agregacao por periodo ou outras colunas, soma das linhas deve bater
  expect_equal(sum(res$n), n_expected)
})

test_that("param_trend() retorna NA (slope/p_value/r2) quando n < 4", {
  skip_on_cran()

  # constrói um dataset mínimo com 3 observacoes
  df <- tibble::tibble(
    data = as.Date(c("2025-01-01","2025-01-05","2025-01-10")),
    ponto = "P1",
    turbidez = c(1.0, 1.5, 1.2)
  )

  res <- param_trend(df, parametro = "turbidez")

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("slope","p_value","r2","n") %in% names(res)))
  expect_true(is.na(res$slope))
  expect_true(is.na(res$p_value))
  expect_true(is.na(res$r2))
  expect_equal(res$n, 3)
})

test_that("param_trend() calcula slope e p_value com dados suficientes", {
  skip_on_cran()

  # serie com 6 pontos e tendencia positiva ~0.5 unidade por dia
  dates <- as.Date("2025-01-01") + 0:5
  y <- 2 + 0.5 * (0:5)  # sem ruido para determinismo
  df <- tibble::tibble(
    data = dates,
    ponto = "P1",
    turbidez = y
  )

  res <- param_trend(df, parametro = "turbidez")

  expect_s3_class(res, "tbl_df")
  expect_false(is.na(res$slope))
  expect_gt(res$slope, 0.4)
  expect_lt(res$slope, 0.6)
  expect_true(!is.na(res$p_value))
  expect_lt(res$p_value, 0.01)
  expect_true(!is.na(res$r2))
  expect_gt(res$r2, 0.95)
  expect_equal(res$n, 6)
})

test_that("param_plot() retorna um ggplot e aceita facet por ponto", {
  skip_on_cran()

  library(tikatuwq)
  data("wq_demo", package = "tikatuwq")

  # escolhe ate 2 pontos (se existirem)
  pts <- head(unique(wq_demo$ponto), 2)
  g <- param_plot(wq_demo, parametro = "turbidez", pontos = pts, add_trend = TRUE, facet = TRUE)

  expect_s3_class(g, "ggplot")
  # nao testamos renderizacao, apenas a classe e que possui camadas basicas
  expect_gte(length(g$layers), 2)  # pontos + linhas pelo menos
})

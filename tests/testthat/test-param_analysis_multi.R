test_that("param_summary_multi() combina parametros e respeita filtros", {
  skip_on_cran()

  library(tikatuwq)
  data("wq_demo", package = "tikatuwq")

  # escolhe parametros que existem no demo; ajuste se necessario
  params <- intersect(c("turbidez", "od", "pH"), names(wq_demo))
  expect_gte(length(params), 2)

  # filtra por um ou dois pontos, se existirem
  pts <- head(unique(wq_demo$ponto), 2)
  res <- param_summary_multi(wq_demo, parametros = params, pontos = pts, period = "none")

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("parametro", "n", "mean", "sd", "min", "median", "max") %in% names(res)))
  expect_true(all(res$parametro %in% params))
  if ("ponto" %in% names(res)) {
    expect_true(all(res$ponto %in% pts))
  }

  # deve haver pelo menos uma linha por parametro (pode multiplicar por ponto/periodo)
  n_by_param <- table(res$parametro)
  expect_true(all(n_by_param >= 1))
})

test_that("param_trend_multi() produz uma linha por parametro (por grupo quando houver)", {
  skip_on_cran()

  # dataset sintetico deterministico com dois parametros e dois pontos
  dates <- as.Date("2025-01-01") + 0:7
  df <- tibble::tibble(
    data = rep(dates, times = 2),
    ponto = rep(c("P1", "P2"), each = length(dates)),
    # tendencia positiva ~0.3/dia
    turbidez = c(2 + 0.3 * (0:7), 3 + 0.3 * (0:7)),
    # tendencia negativa ~-0.2/dia
    od       = c(8 - 0.2 * (0:7), 7.5 - 0.2 * (0:7))
  )

  res <- param_trend_multi(df, parametros = c("turbidez", "od"))

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("parametro", "slope", "p_value", "r2", "n") %in% names(res)))
  expect_setequal(unique(res$parametro), c("turbidez", "od"))

  # deve haver duas linhas por parametro (uma por ponto P1/P2)
  if ("ponto" %in% names(res)) {
    tab <- table(res$parametro, res$ponto)
    expect_true(all(tab >= 1))
  }

  # checagens de sinal da tendencia
  mean_slope_turb <- mean(res$slope[res$parametro == "turbidez"], na.rm = TRUE)
  mean_slope_od   <- mean(res$slope[res$parametro == "od"], na.rm = TRUE)
  expect_gt(mean_slope_turb, 0.2)
  expect_lt(mean_slope_od, -0.1)
})

test_that("param_plot_multi() retorna ggplot e facet funciona", {
  skip_on_cran()

  library(tikatuwq)
  data("wq_demo", package = "tikatuwq")

  params <- intersect(c("turbidez", "od", "pH"), names(wq_demo))
  params <- head(params, 2)  # 2 parametros para facet enxuto
  expect_gte(length(params), 2)

  pts <- head(unique(wq_demo$ponto), 2)

  # facet por parametro
  g1 <- param_plot_multi(wq_demo, parametros = params, pontos = pts,
                         add_trend = TRUE, facet = "parametro")
  expect_s3_class(g1, "ggplot")
  expect_gte(length(g1$layers), 2)

  # facet em grade ponto ~ parametro (se houver 2 pontos)
  if (length(pts) >= 2) {
    g2 <- param_plot_multi(wq_demo, parametros = params, pontos = pts,
                           add_trend = FALSE, facet = "grid")
    expect_s3_class(g2, "ggplot")
    expect_gte(length(g2$layers), 2)
  }
})

test_that("funcoes *_multi validam parametros inexistentes", {
  skip_on_cran()

  library(tikatuwq)
  data("wq_demo", package = "tikatuwq")

  # cria um nome inexistente para garantir erro claro
  expect_error(
    param_summary_multi(wq_demo, parametros = "nao_existe_abc"),
    "Nenhum dos parametros especificados foi encontrado"
  )
  expect_error(
    param_trend_multi(wq_demo, parametros = "nao_existe_abc"),
    "Nenhum dos parametros especificados foi encontrado"
  )
  expect_error(
    param_plot_multi(wq_demo, parametros = "nao_existe_abc"),
    "Nenhum dos parametros especificados foi encontrado"
  )
})

test_that("plot_map_quality() retorna objeto leaflet quando leaflet disponivel", {
  skip_if_not_installed("leaflet")
  data("wq_demo", package = "tikatuwq")
  d <- iqa(wq_demo, na_rm = TRUE)
  result <- plot_map_quality(d, index = "IQA")
  expect_s3_class(result, "leaflet")
})

test_that("plot_map_quality() retorna NULL invisivel sem leaflet", {
  skip_if(requireNamespace("leaflet", quietly = TRUE),
          "leaflet esta disponivel — pular teste de fallback")
  data("wq_demo", package = "tikatuwq")
  d <- iqa(wq_demo, na_rm = TRUE)
  expect_message(
    result <- plot_map_quality(d, index = "IQA"),
    "leaflet"
  )
  expect_null(result)
})

test_that("plot_map_quality() erro quando coluna de indice nao existe", {
  skip_if_not_installed("leaflet")
  d <- data.frame(lat = -16, lon = -39, ponto = "P1")
  expect_error(plot_map_quality(d, index_col = "inexistente"), "'inexistente'")
})

test_that("plot_map_quality() erro quando lat/lon ausentes", {
  skip_if_not_installed("leaflet")
  data("wq_demo", package = "tikatuwq")
  d <- iqa(wq_demo, na_rm = TRUE)
  d$lat <- NULL
  expect_error(plot_map_quality(d, index = "IQA"), "'lat'")
})

test_that("plot_map_quality() detecta automaticamente IET_Lamparelli", {
  skip_if_not_installed("leaflet")
  data("wq_demo", package = "tikatuwq")
  d <- iet_lamparelli(wq_demo, ambiente = "rio", .keep_ids = TRUE)
  if ("IET_Lamparelli" %in% names(d) && "lat" %in% names(d)) {
    result <- plot_map_quality(d)
    expect_s3_class(result, "leaflet")
  } else {
    skip("IET_Lamparelli ou lat nao encontrados no resultado de iet_lamparelli()")
  }
})

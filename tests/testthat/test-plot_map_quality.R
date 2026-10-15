test_that("plot_map_quality() retorna objeto leaflet quando leaflet disponivel", {
  skip_if_not_installed("leaflet")
  data("wq_demo", package = "tikatuwq")
  d <- iqa(wq_demo, allow_partial = TRUE)
  result <- plot_map_quality(d, index = "IQA")
  expect_s3_class(result, "leaflet")
})

test_that("plot_map_quality() retorna NULL invisivel sem leaflet", {
  skip_if(requireNamespace("leaflet", quietly = TRUE),
          "leaflet esta disponivel — pular teste de fallback")
  data("wq_demo", package = "tikatuwq")
  d <- iqa(wq_demo, allow_partial = TRUE)
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
  d <- iqa(wq_demo, allow_partial = TRUE)
  d$lat <- NULL
  expect_error(plot_map_quality(d, index = "IQA"), "'lat'")
})

test_that("plot_map_quality() detecta automaticamente IET_Lamparelli", {
  skip_if_not_installed("leaflet")
  data("wq_demo", package = "tikatuwq")
  d <- iet_lamparelli(wq_demo, ambiente = "rio", .keep_ids = TRUE)
  # iet_lamparelli() devolve IET_Lamp; ate a v0.10.0 o mapa so procurava
  # IET_Lamparelli e este teste era pulado em silencio.
  expect_true(all(c("IET_Lamp", "lat") %in% names(d)))
  result <- plot_map_quality(d)
  expect_s3_class(result, "leaflet")
})

test_that("plot_map valida colunas e coordenadas", {
  skip_on_cran()

  # faltando latitude/longitude -> deve erro
  expect_error(plot_map(data.frame(x = 1)), "latitude/longitude")

  # dados com 1 coordenada invalida (NA em latitude)
  df <- data.frame(
    rio       = c("R1", "R2"),
    ponto     = c("P1", "P2"),
    latitude  = c(-12.1, NA),
    longitude = c(-38.5, -38.6),
    iqa       = c(70, 50)
  )

  # a função deve avisar que removeu coordenadas invalidas
  expect_warning(
    m <- plot_map(df),
    regexp = "coordenad(as|es)|removid(o|a)s|invalid|removed",
    ignore.case = TRUE
  )

  expect_true(inherits(m, "leaflet"))
})

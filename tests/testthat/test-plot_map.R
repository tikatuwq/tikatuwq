test_that("plot_map valida colunas e coordenadas", {
  df <- data.frame(
    rio = c("R1","R2"),
    ponto = c("P1","P2"),
    latitude = c(-12.1, NA),
    longitude = c(-38.5, -38.6),
    iqa = c(70, 50)
  )

  expect_error(plot_map(data.frame(x=1)), "latitude/longitude")

  m <- plot_map(df)
  expect_true(inherits(m, "leaflet"))
})



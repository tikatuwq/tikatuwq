test_that("plot_map errors when leaflet not available", {
  skip_if_not_installed("leaflet")
  skip_on_cran()
  df <- data.frame(x = 1:3)
  expect_error(
    plot_map(df),
    "latitude/longitude"
  )
})

test_that("plot_map accepts lat/lon and warns on invalid coordinates", {
  skip_if_not_installed("leaflet")
  skip_on_cran()
  df <- data.frame(
    rio   = c("R1", "R2"),
    ponto = c("P1", "P2"),
    lat   = c(-12.1, NA),
    lon   = c(-38.5, -38.6),
    iqa   = c(70, 50)
  )
  expect_warning(
    m <- plot_map(df),
    regexp = "coordenad(as|es)|removid(o|a)s|invalid|removed",
    ignore.case = TRUE
  )
  expect_true(inherits(m, "leaflet"))
})

test_that("plot_map accepts latitude/longitude as aliases for lat/lon", {
  skip_if_not_installed("leaflet")
  skip_on_cran()
  df <- data.frame(
    rio       = c("R1", "R2"),
    ponto     = c("P1", "P2"),
    latitude  = c(-12.1, -12.2),
    longitude = c(-38.5, -38.6),
    iqa       = c(70, 50)
  )
  m <- plot_map(df)
  expect_true(inherits(m, "leaflet"))
})


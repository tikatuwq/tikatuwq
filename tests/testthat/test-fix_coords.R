test_that("fix_coords normalizes coordinates when outside valid range", {
  # Coordinates in degrees * 1e7
  df <- data.frame(
    lat = c(-155432345, -16.5, 16.5),
    lon = c(-393212345, -39.3, 39.3),
    ponto = c("P1", "P2", "P3")
  )
  out <- fix_coords(df)
  
  # First row should be normalized
  expect_true(abs(out$lat[1]) < 90)
  expect_true(abs(out$lon[1]) < 180)
  
  # Valid coordinates should remain unchanged
  expect_equal(out$lat[2], -16.5)
  expect_equal(out$lon[2], -39.3)
})

test_that("fix_coords handles missing lat/lon columns", {
  df <- data.frame(ponto = c("P1", "P2"), ph = c(7, 7.2))
  expect_equal(fix_coords(df), df)
})

test_that("fix_coords invalidates coordinates outside valid range", {
  df <- data.frame(lat = c(-16.5, 100), lon = c(-39.3, 200))
  out <- fix_coords(df)
  
  # Invalid coordinates should become NA
  expect_true(is.na(out$lat[2]))
  expect_true(is.na(out$lon[2]))
  
  # Valid coordinates should remain
  expect_equal(out$lat[1], -16.5)
  expect_equal(out$lon[1], -39.3)
})

test_that("fix_coords errors on non-data.frame input", {
  expect_error(fix_coords("not a df"), "must be a data.frame")
})

test_that("fix_coords accepts custom column names", {
  df <- data.frame(latitude = -155432345, longitude = -393212345)
  out <- fix_coords(df, lat = "latitude", lon = "longitude")
  expect_true(abs(out$latitude) < 90)
  expect_true(abs(out$longitude) < 180)
})


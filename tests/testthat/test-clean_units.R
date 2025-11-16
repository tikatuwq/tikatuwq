test_that("clean_units validates ranges and warns on outliers", {
  # Normal data should return unchanged
  df <- data.frame(ph = c(7, 7.2), od = c(6.5, 7.0), p_total = c(0.05, 0.08))
  expect_equal(clean_units(df), df)
  
  # pH outside range should warn
  df_bad_ph <- data.frame(ph = c(7, 20, 7.2))
  expect_warning(clean_units(df_bad_ph), "pH values outside typical range")
  
  # OD outside range should warn
  df_bad_od <- data.frame(od = c(6.5, 25, 7.0))
  expect_warning(clean_units(df_bad_od), "OD values outside typical range")
  
  # Negative values should warn
  df_neg <- data.frame(turbidez = c(5, -2, 10), p_total = c(0.05, -0.01, 0.08))
  expect_warning(clean_units(df_neg), "Negative turbidity values")
  expect_warning(clean_units(df_neg), "Negative p_total values")
})

test_that("clean_units handles missing columns gracefully", {
  df <- data.frame(ph = c(7, 7.2))
  expect_equal(clean_units(df), df)
})

test_that("clean_units errors on non-data.frame input", {
  expect_error(clean_units("not a df"), "must be a data.frame or tibble")
  expect_error(clean_units(NULL), "must be a data.frame or tibble")
})

test_that("clean_units converts units when units_map is provided", {
  # Testa conversao ug/L -> mg/L para p_total
  df <- data.frame(p_total = c(50, 100, 150))  # em ug/L
  result <- clean_units(df, units_map = list(p_total = "ug/L"))
  
  expect_true(is.numeric(result$p_total))
  expect_equal(result$p_total[1], 0.05)  # 50 ug/L = 0.05 mg/L
  expect_equal(result$p_total[2], 0.10)  # 100 ug/L = 0.10 mg/L
  expect_equal(result$p_total[3], 0.15)  # 150 ug/L = 0.15 mg/L
})

test_that("clean_units warns on suspicious ranges suggesting wrong units", {
  # p_total muito alto pode ser ug/L
  df <- data.frame(p_total = c(100, 200))  # suspeito se em mg/L
  expect_warning(clean_units(df), "p_total values >5 mg/L")
  
  # temperatura em Kelvin
  df_k <- data.frame(temperatura = c(290, 295))  # Kelvin
  expect_warning(clean_units(df_k), "Temperatura values suggest Kelvin")
})


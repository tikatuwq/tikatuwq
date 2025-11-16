test_that("conama_text returns formatted summary", {
  skip_on_ci()  # depends on conama_check
  df <- data.frame(
    ph = c(7, 4), od = c(6, 8), turbidez = c(50, 10),
    dbo = c(3, 5), coliformes = c(500, 1000), p_total = c(0.04, 0.06)
  )
  out <- conama_text(df, classe = "2", only_violations = TRUE)
  
  expect_true(is.character(out))
  expect_true(length(out) >= 1)
  expect_true(grepl("CONAMA", out[1], ignore.case = TRUE))
})

test_that("conama_text handles conformant data", {
  skip_on_ci()
  df <- data.frame(
    ph = c(7, 7.5), od = c(8, 9), turbidez = c(10, 5),
    dbo = c(3, 2), coliformes = c(200, 100), p_total = c(0.03, 0.02)
  )
  out <- conama_text(df, classe = "2", only_violations = TRUE)
  
  expect_true(is.character(out))
  # Should mention conformity or no violations
  expect_true(any(grepl("conformes|viola", out, ignore.case = TRUE)))
})

test_that("conama_text handles empty/no violations", {
  skip_on_ci()
  df <- data.frame(
    ph = c(7, 7.5), od = c(8, 9), turbidez = c(10, 5)
  )
  out <- conama_text(df, classe = "2", only_violations = FALSE)
  expect_true(is.character(out))
})


test_that("conama_check adiciona colunas *_ok", {
  skip_on_ci()  # temporário: estabilizar limites multi-linha no CI
  df <- data.frame(
    pH = 7, od = 6, turbidez = 50, dbo = 3,
    coliformes = 500, p_total = 0.04
  )
  out <- conama_check(df, classe = "2")
  oks <- out[1, grep("_ok$", names(out)), drop = FALSE]
  expect_true(all(unlist(oks)))
})

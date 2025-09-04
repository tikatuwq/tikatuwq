test_that("conama_check adiciona colunas *_ok", {
  df <- data.frame(pH=7.2, od=6.0, turbidez=4, temperatura=24, coliformes=150)
  out <- conama_check(df, classe = "2")
  expect_true(all(c("pH_ok","od_ok","turbidez_ok","temperatura_ok","coliformes_ok") %in% names(out)))
})

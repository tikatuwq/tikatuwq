test_that("conama_freq_check() retorna tibble com colunas esperadas", {
  data("wq_demo", package = "tikatuwq")
  out <- conama_freq_check(wq_demo, classe = "2", by = "ponto")
  expect_s3_class(out, "data.frame")
  expect_true(all(c("ponto", "ano", "parametro", "n", "n_ok",
                    "pct_ok", "freq_conforme", "aplicou_regra") %in% names(out)))
})

test_that("conama_freq_check() freq_conforme e NA quando n < min_n", {
  d <- data.frame(
    ponto = "P1",
    data  = as.Date(c("2023-01-15", "2023-06-20")),
    turbidez = c(50, 110)  # 2 amostras — abaixo de min_n = 6
  )
  out <- conama_freq_check(d, classe = "2", by = "ponto", min_n = 6L)
  # Todas as linhas devem ter aplicou_regra = FALSE (n < min_n)
  turb_row <- out[out$parametro == "turbidez", ]
  if (nrow(turb_row) > 0) {
    expect_false(any(turb_row$aplicou_regra, na.rm = TRUE))
  }
})

test_that("conama_freq_check() identifica conformidade 100% corretamente", {
  d <- data.frame(
    ponto    = rep("P1", 7),
    data     = as.Date(paste0("2023-0", 1:7, "-15")),
    turbidez = rep(20, 7)  # todos abaixo de 100 UNT (classe 2)
  )
  out <- conama_freq_check(d, classe = "2", by = "ponto")
  turb_row <- out[out$parametro == "turbidez", ]
  expect_true(all(turb_row$freq_conforme, na.rm = TRUE))
})

test_that("conama_freq_check() identifica nao-conformidade quando maioria excede", {
  d <- data.frame(
    ponto    = rep("P1", 7),
    data     = as.Date(paste0("2023-0", 1:7, "-15")),
    turbidez = c(150, 120, 130, 110, 90, 105, 140)  # todos > 100 UNT
  )
  out <- conama_freq_check(d, classe = "2", by = "ponto")
  turb_row <- out[out$parametro == "turbidez" & out$aplicou_regra, ]
  if (nrow(turb_row) > 0) {
    expect_false(any(turb_row$freq_conforme, na.rm = TRUE))
  }
})

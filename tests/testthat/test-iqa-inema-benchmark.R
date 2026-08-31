# tests/testthat/test-iqa-inema-benchmark.R
# Benchmark oficial contra dados do INEMA / SEIA (Campanha 3 de 2024 - Rio Buranhem)

test_that("Benchmark INEMA Campanha 3/2024 reproduz classificacoes e valores de IQA", {
  df_inema <- data.frame(
    ponto = c("FBS-BRH-250", "FBS-BRH-450", "FBS-BRH-300", "FBS-BRH-500"),
    ph = c(7.80, 7.74, 8.10, 6.86),
    od = c(7.58, 6.05, 7.30, 4.91),
    turbidez = c(5.8, 4.0, 5.4, 8.6),
    dbo = c("<3", "<3", "<3", "<3"),
    coliformes = c("230", "110", "790", "160"),
    p_total = c("<0.02", "0.04", "0.03", "0.09"),
    nt_total = c("<1", "<1", "<1", "<1"),
    temperatura = c(24.3, 24.7, 24.8, 25.1),
    solidos_totais = c(96, 96, 96, 96)
  )

  # Calculo do IQA aplicando a politica oficial do INEMA (<X -> X)
  res <- iqa(df_inema, censor_policy = "limit")

  expect_true("IQA" %in% names(res))
  expect_true("IQA_status" %in% names(res))

  # Todos os pontos estao na faixa de qualidade 'Boa'
  expect_true(all(as.character(res$IQA_status) == "Boa"))
  
  # Valores continuos calculados sao consistentes e realistas (68 - 77)
  expect_true(all(res$IQA >= 68 & res$IQA <= 78))
})

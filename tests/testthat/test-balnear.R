test_that("balnear_check() retorna tibble com colunas esperadas", {
  data("wq_demo", package = "tikatuwq")
  out <- balnear_check(wq_demo, by = "ponto")
  expect_s3_class(out, "data.frame")
  expect_true(all(c("indicador","n_amostras","n_avaliadas",
                    "classificacao","propria","amostras_insuficientes") %in% names(out)))
})

test_that("balnear_check() classifica como Excelente quando todos abaixo de 250", {
  d <- data.frame(
    ponto      = rep("Praia", 6),
    data       = as.Date(paste0("2023-0", 1:6, "-01")),
    coliformes = rep(100, 6)  # todos <= 250 NMP/100mL
  )
  out <- balnear_check(d, by = "ponto")
  expect_equal(out$classificacao, "Excelente")
  expect_true(out$propria)
})

test_that("balnear_check() classifica como Impropria quando maioria excede 1000", {
  d <- data.frame(
    ponto      = rep("Praia", 6),
    data       = as.Date(paste0("2023-0", 1:6, "-01")),
    coliformes = c(1500, 2000, 1800, 1200, 1600, 900)  # 5/6 > 1000
  )
  out <- balnear_check(d, by = "ponto")
  expect_equal(out$classificacao, "Impropria")
  expect_false(out$propria)
})

test_that("balnear_check() detecta amostras_insuficientes corretamente", {
  d <- data.frame(
    ponto      = rep("P1", 3),
    data       = as.Date(c("2023-01-01","2023-02-01","2023-03-01")),
    coliformes = c(100, 150, 200)
  )
  out <- balnear_check(d, by = "ponto", n_samples = 5L)
  expect_true(out$amostras_insuficientes)
})

test_that("balnear_check() classifica Satisfatoria corretamente", {
  d <- data.frame(
    ponto      = rep("P1", 6),
    data       = as.Date(paste0("2023-0", 1:6, "-01")),
    coliformes = c(600, 700, 800, 900, 300, 200)  # todos <= 1000; maioria > 500
  )
  out <- balnear_check(d, by = "ponto")
  expect_true(out$propria)
  expect_true(out$classificacao %in% c("Satisfatoria","Muito Boa"))
})

test_that("balnear_check() usa e_coli com limites corretos (200/400/800)", {
  d <- data.frame(
    ponto  = rep("P1", 6),
    data   = as.Date(paste0("2023-0", 1:6, "-01")),
    e_coli = rep(150, 6)  # todos <= 200 E. coli
  )
  out <- balnear_check(d, col = "e_coli", by = "ponto")
  expect_equal(out$classificacao, "Excelente")
})

test_that("balnear_check() erro quando coluna nao e detectada", {
  d <- data.frame(ponto = "P1", turbidez = 10)
  expect_error(balnear_check(d), "nao detectada")
})

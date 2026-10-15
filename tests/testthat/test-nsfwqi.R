# tests/testthat/test-nsfwqi.R
# nsfwqi() e uma aproximacao por funcoes-degrau (nao e o NSF WQI validado).
# Estes testes fixam o comportamento documentado, inclusive as limitacoes.

nsf_d <- function() {
  data.frame(od = 8, coliformes = 10, ph = 7.5, dbo = 1, temperatura = 25,
             p_ortofosfato = 0.01, n_nitrato = 0.2, turbidez = 2, solidos_totais = 50)
}

test_that("pesos padrao somam 1 e as duas agregacoes funcionam", {
  w <- formals(nsfwqi)$pesos
  expect_equal(sum(eval(w)), 1)
  g <- nsfwqi(nsf_d())
  a <- nsfwqi(nsf_d(), aggregation = "brown1970_arithmetic")
  expect_equal(attr(g, "nsfwqi_aggregation"), "mcclelland_geometric")
  expect_equal(attr(a, "nsfwqi_aggregation"), "brown1970_arithmetic")
  expect_equal(attr(g, "nsfwqi_validation"), "approximate_step_functions_not_validated")
})

test_that("limitacao documentada: indice maximo e 90 (Qi maximo dos degraus)", {
  g <- nsfwqi(nsf_d())
  expect_lte(g$NSFWQI, 90)
  expect_equal(g$NSFWQI, 89.7, tolerance = 0.2)   # ts = 85 no melhor degrau
  expect_false(identical(g$NSFWQI_status, "Excelente"))
})

test_that("solidos suspensos nao substituem solidos totais", {
  d <- nsf_d(); d$solidos_totais <- NULL; d$solidos_suspensos <- 40
  o <- suppressWarnings(nsfwqi(d))
  expect_true(is.na(o$NSFWQI))                     # estrito: falta componente
  w <- character(0)
  withCallingHandlers(nsfwqi(d), warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning")
  })
  expect_true(any(grepl("suspended solids", w)))
  p <- suppressWarnings(nsfwqi(d, na_rm = TRUE))
  expect_false(is.na(p$NSFWQI))                    # parcial, sem ts
})

test_that("modo estrito: componente sem coluna resulta em NA, com aviso", {
  d <- nsf_d(); d$n_nitrato <- NULL
  expect_warning(o <- nsfwqi(d), "no column for component")
  expect_true(is.na(o$NSFWQI))
  expect_warning(p <- nsfwqi(d, na_rm = TRUE), "renormalized")
  expect_false(is.na(p$NSFWQI))
})

test_that("p_total (P) e convertido para PO4 quando nao ha ortofosfato", {
  d <- nsf_d(); d$p_ortofosfato <- NULL; d$p_total <- 0.01
  expect_equal(nsfwqi(d)$NSFWQI, nsfwqi(nsf_d())$NSFWQI)
})

test_that("data frame vazio", {
  e <- nsfwqi(nsf_d()[0, ])
  expect_equal(nrow(e), 0L)
})

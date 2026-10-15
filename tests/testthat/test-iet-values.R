# tests/testthat/test-iet-values.R
# Valores de referencia do IET. Ate a v0.10.0 nao havia teste numerico e as
# duas funcoes usavam log10 (Carlson) ou formula sem fonte (Lamparelli).

test_that("Lamparelli reproduz os cortes das Tabelas 5 e 6 (CETESB 2023, Apendice D)", {
  # Rios (Tabela 5): limites superiores de P-total e clorofila a de cada classe
  # correspondem a IET 47 / 52 / 59 / 63 / 67 (tolerancia 0.6: a tabela
  # arredonda as concentracoes).
  cortes <- c(47, 52, 59, 63, 67)
  rio <- iet_lamparelli(tp = c(13, 35, 137, 296, 640) / 1000, ambiente = "rio", add_status = FALSE)
  expect_lte(max(abs(rio$IET_TP - cortes)), 0.6)
  rio_cl <- iet_lamparelli(chla = c(0.74, 1.31, 2.96, 4.70, 7.46), ambiente = "rio", add_status = FALSE)
  expect_lte(max(abs(rio_cl$IET_Chla - cortes)), 0.6)

  res <- iet_lamparelli(tp = c(8, 19, 52, 120, 233) / 1000, ambiente = "reservatorio", add_status = FALSE)
  expect_lte(max(abs(res$IET_TP - cortes)), 0.6)
  res_cl <- iet_lamparelli(chla = c(1.17, 3.24, 11.03, 30.55, 69.05), ambiente = "reservatorio", add_status = FALSE)
  expect_lte(max(abs(res_cl$IET_Chla - cortes)), 0.6)
})

test_that("Lamparelli: ambiente muda o resultado e o agregado e a media de PT e CL", {
  r <- iet_lamparelli(tp = 0.05, chla = 3, ambiente = "rio", add_status = FALSE)
  v <- iet_lamparelli(tp = 0.05, chla = 3, ambiente = "reservatorio", add_status = FALSE)
  expect_false(isTRUE(all.equal(r$IET_Lamp, v$IET_Lamp)))
  expect_equal(r$IET_Lamp, mean(c(r$IET_TP, r$IET_Chla)))

  # Secchi e auxiliar: nao entra no agregado
  s <- iet_lamparelli(tp = 0.05, chla = 3, sd = 1, ambiente = "rio", add_status = FALSE)
  expect_equal(s$IET_Lamp, r$IET_Lamp)
  expect_equal(s$IET_Secchi, 60)
  expect_warning(o <- iet_lamparelli(sd = 1, add_status = FALSE), "transparency")
  expect_true(is.na(o$IET_Lamp))
})

test_that("Lamparelli: modo data.frame equivale ao vetorial (p_total em mg/L)", {
  d <- data.frame(p_total = c(0.02, 0.1), clorofila = c(1, 10))
  a <- iet_lamparelli(d, ambiente = "reservatorio", add_status = FALSE)
  b <- iet_lamparelli(tp = c(0.02, 0.1), chla = c(1, 10), ambiente = "reservatorio", add_status = FALSE)
  expect_equal(a$IET_Lamp, b$IET_Lamp)
})

test_that("Carlson usa logaritmo natural (TSI = 10 (6 - log2 SD))", {
  # Valores de definicao: SD = 1 m -> 60; TP = 48 ug/L -> 60; Chl = 20 ug/L -> ~60
  o <- iet_carlson(secchi = c(1, 2), clorofila = 20, tp = 48, add_status = FALSE)
  expect_equal(o$TSI_Secchi, c(60, 50), tolerance = 0.05)
  expect_equal(o$TSI_TP[1], 60, tolerance = 0.05)
  expect_equal(o$TSI_Chla[1], 60, tolerance = 0.1)
})

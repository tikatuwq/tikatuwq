# tests/testthat/test-iqa-igam-compare.R
# Metodo IGAM (bloqueado), API method/classification e compare_iqa().
# Os resultados "sinteticos" abaixo testam a montagem e as estatisticas do
# comparador; nao sao medicoes reais nem valores IGAM.

d_cetesb <- function() {
  data.frame(
    ponto = c("P1", "P2", "P3"),
    od = c(6.5, 4, 7), temperatura = 25, dbo = c(2, 6, NA), coliformes = c(200, 3000, 50),
    ph = c(7.2, 7.8, 7), nt_total = c(1.2, 4, 1), p_total = c(0.05, 0.2, 0.03),
    turbidez = c(8, 40, 5), solidos_totais = c(120, 250, 90)
  )
}

test_that("method = 'IGAM' esta declarado mas bloqueado, com erro diagnostico", {
  d <- d_cetesb()
  e <- tryCatch(iqa(d, method = "IGAM"), error = function(e) e)
  expect_s3_class(e, "tikatuwq_method_unavailable")
  expect_match(conditionMessage(e), "SCQA")
  # Diagnostico das entradas: NT nao substitui nitrato; P nao e convertido
  expect_match(conditionMessage(e), "nt_total")
  expect_match(conditionMessage(e), "p_total")
  expect_error(iqa_components(d, method = "IGAM"), class = "tikatuwq_method_unavailable")
  m <- iqa_methods()
  expect_false(m$available[m$method == "IGAM"])
  expect_true(m$available[m$method == "CETESB"])
})

test_that("Especificacao IGAM: pesos, obrigatorios, temperatura e classes", {
  s <- .iqa_igam_spec
  expect_equal(sum(s$weights), 1)
  expect_setequal(s$mandatory, c("od", "microbiologico"))
  expect_equal(s$temperature_qs, 92)
  expect_null(s$qi_equations)
  # Classes do relatorio IGAM 2017 (p. 9): Muito Ruim [0,25], Ruim (25,50],
  # Medio (50,70], Bom (70,90], Excelente (90,100]
  expect_equal(as.character(classify_iqa(c(25, 25.01, 50, 50.01, 70, 70.01, 90, 90.01), "igam")),
               c("Muito Ruim", "Ruim", "Ruim", "Medio", "Medio", "Bom", "Bom", "Excelente"))
})

test_that("IGAM: peso de ausente e repartido igualmente; sem OD/microbiologico nao calcula", {
  w <- .iqa_igam_spec$weights
  all_ok <- setNames(rep(TRUE, 9), names(w))
  expect_equal(.igam_effective_weights(all_ok), w)
  no_turb <- all_ok; no_turb["turbidez"] <- FALSE
  ew <- .igam_effective_weights(no_turb)
  expect_equal(sum(ew), 1)
  expect_equal(unname(ew["turbidez"]), 0)
  # 0.08 repartido igualmente entre os 8 restantes: +0.01 cada
  expect_equal(unname(ew["od"]), 0.18)
  expect_equal(unname(ew["solidos_totais"]), 0.09)
  no_od <- all_ok; no_od["od"] <- FALSE
  expect_null(.igam_effective_weights(no_od))
  no_mic <- all_ok; no_mic["microbiologico"] <- FALSE
  expect_null(.igam_effective_weights(no_mic))
})

test_that("IGAM: entradas nao sao substituidas por analitos diferentes", {
  d <- data.frame(od = 7, coliformes = 10, ph = 7, dbo = 2, nt_total = 1,
                  p_ortofosfato = 0.1, turbidez = 5, tds = 50)
  p <- .igam_check_inputs(d)$problems
  expect_true(any(grepl("total nitrogen", p)))
  expect_true(any(grepl("orthophosphate", p)))
  expect_true(any(grepl("TDS", p)))
  ok <- data.frame(od = 7, coliformes = 10, ph = 7, dbo = 2, nitrato = 1,
                   fosfato_total = 0.1, turbidez = 5, solidos_totais = 50)
  expect_length(.igam_check_inputs(ok)$problems, 0)
})

test_that("iqa(): escala segue method; classification e depreciado", {
  d <- d_cetesb()
  o <- iqa(d)
  expect_equal(attr(o, "iqa_classification"), "cetesb")
  expect_equal(attr(o, "iqa_calculation_method"), "CETESB")
  expect_false("cross_classification" %in% attr(o, "iqa_nonstandard"))
  expect_warning(x <- iqa(d, classification = "igam"), "Cross-classification")
  expect_identical(x$IQA, o$IQA)
  expect_true("cross_classification" %in% attr(x, "iqa_nonstandard"))
  expect_warning(iqa(d, classification = "cetesb"), "deprecated")
  # classify_iqa() continua disponivel
  expect_equal(as.character(classify_iqa(80, classification = "igam")), "Bom")
})

test_that("compare_iqa(): IGAM indisponivel deixa todas as linhas inelegiveis", {
  d <- d_cetesb()
  cmp <- compare_iqa(d, id_cols = "ponto")
  expect_equal(nrow(cmp), 3L)
  expect_equal(cmp$ponto, d$ponto)
  expect_equal(cmp$IQA_CETESB, iqa(d)$IQA)
  expect_false(any(cmp$elegivel_IGAM))
  expect_true(all(grepl("indisponivel", cmp$motivo_IGAM)))
  # P3 sem DBO: inelegivel para CETESB com motivo especifico
  expect_false(cmp$elegivel_CETESB[3])
  expect_match(cmp$motivo_CETESB[3], "dbo")
  s <- attr(cmp, "summary")
  expect_equal(s$n_total, 3L)
  expect_equal(s$n_referencia, 2L)
  expect_equal(s$n_paired, 0L)
  expect_true(is.na(s$mean_diff))
  expect_false(attr(cmp, "methods_available")[["IGAM"]])
  expect_identical(d, d_cetesb())   # df nao modificado
})

test_that("compare_iqa(): validacoes de argumentos e identificadores", {
  d <- d_cetesb()
  expect_error(compare_iqa(d, methods = "CETESB"), "two or more")
  expect_error(compare_iqa(d, methods = c("CETESB", "NSF_approx")), "not comparable")
  expect_error(compare_iqa(d, methods = c("CETESB", "ANA")), "not comparable")
  expect_error(compare_iqa(d, allow_partial = TRUE), "not allowed")
  expect_error(compare_iqa(d, id_cols = "nao_existe"), "not found")
  d2 <- d; d2$ponto <- "P1"
  expect_error(compare_iqa(d2, id_cols = "ponto"), "share the same")
  e <- compare_iqa(d[0, ])
  expect_equal(nrow(e), 0L)
  expect_equal(attr(e, "summary")$n_total, 0L)
  one <- compare_iqa(d[1, ], details = TRUE)
  expect_equal(nrow(one), 1L)
  expect_true(is.list(attr(one, "components")))
})

test_that(".compare_assemble(): estatisticas so nos pares validos (resultados sinteticos)", {
  mk <- function(raw, cls) list(IQA = round(raw, 2), IQA_raw = raw, classe = cls,
                                elegivel = !is.na(raw), motivo = ifelse(is.na(raw), "x", NA),
                                components = NULL, available = TRUE)
  a <- mk(c(50, 60, 70, NA, 80), c("Regular", "Boa", "Boa", NA, "Otima"))
  b <- mk(c(55, 58, NA, 40, 86), c("Medio", "Medio", NA, "Ruim", "Bom"))
  out <- .compare_assemble(data.frame(.row = 1:5), list(CETESB = a, IGAM = b))
  expect_equal(out$par_valido, c(TRUE, TRUE, FALSE, FALSE, TRUE))
  expect_equal(out$delta_IGAM_minus_CETESB, c(5, -2, NA, NA, 6))
  s <- attr(out, "summary")
  expect_equal(s$n_referencia, 4L)
  expect_equal(s$n_metodo, 4L)
  expect_equal(s$n_paired, 3L)
  expect_equal(s$mean_diff, mean(c(5, -2, 6)))
  expect_equal(s$mae, mean(c(5, 2, 6)))
  expect_equal(s$rmse, sqrt(mean(c(25, 4, 36))))
  expect_equal(s$cor_pearson, cor(c(50, 60, 80), c(55, 58, 86)))
  # Transicoes de classe em vez de "concordancia" por rotulo
  tr <- attr(out, "class_transitions")$IGAM_minus_CETESB
  expect_equal(sum(tr$n), 3L)
  expect_false("concordancia" %in% names(s))
  # Menos de 3 pares: correlacao NA
  out2 <- .compare_assemble(data.frame(.row = 1:2),
                            list(CETESB = mk(c(50, 60), c("Regular", "Boa")),
                                 IGAM = mk(c(55, 58), c("Medio", "Medio"))))
  expect_true(is.na(attr(out2, "summary")$cor_pearson))
})

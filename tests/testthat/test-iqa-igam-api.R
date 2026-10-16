# tests/testthat/test-iqa-igam-api.R
# Metodo IGAM liberado em iqa(): fronteiras dos nove subindices (abaixo, igual
# e acima de cada limiar), reproducao dos 49 IQAs publicados pela API publica,
# tratamento de entradas e compare_iqa() com os dois metodos disponiveis.

eps <- 1e-9

# Segmentos transcritos de novo a partir do SCQA 2005 (inst/audit/
# SCQA_FONTES_E_EQUACOES.md), independentes do codigo do pacote.
seg <- list(
  od_le100 = function(x) {
    y1 <- 0.01396 * x + 0.0873; y2 <- pi / 56 * (x - 27); y3 <- pi / 85 * (x - 15)
    100 * sin(y1)^2 - (2.5 * sin(y2) - 0.018 * x + 6.86) * sin(y3) +
      12 / (exp((x - 65) / 10) + exp((65 - x) / 10))
  },
  od_100_140 = function(x) -0.00777142857142832 * x^2 + 1.27854285714278 * x + 49.8817148572,
  cf = function(x) { L <- log10(x); 98.24034 - 34.7145 * L + 2.614267 * L^2 + 0.107821 * L^3 },
  ph_a = function(p) -37.1085 + 41.91277 * p - 15.7043 * p^2 + 2.417486 * p^3 - 0.091252 * p^4,
  ph_b = function(p) -4.69365 - 21.4593 * p - 68.4561 * p^2 + 21.638886 * p^3 - 1.59165 * p^4,
  ph_c = function(p) -7698.19 + 3262.031 * p - 499.494 * p^2 + 33.1551 * p^3 - 0.810613 * p^4,
  dbo = function(x) 100.9571 - 10.7121 * x + 0.49544 * x^2 - 0.011167 * x^3 + 0.0001 * x^4,
  no3_a = function(x) -5.1 * x + 100.17,
  no3_b = function(x) -22.853 * log(x) + 101.18,
  no3_c = function(x) 1e10 * x^-5.1161,
  po4 = function(x) 79.7 * (x + 0.821)^-1.15,
  tu = function(t) 90.37 * exp(-0.0169 * t) - 1.5 * cos(0.0571 * (t - 30)) + 10.22 * exp(-0.231 * t) - 0.8,
  st = function(s) 133.17 * exp(-0.0027 * s) - 53.17 * exp(-0.0141 * s) - 6.2 * exp(-0.00462 * s) * sin(0.0146 * s)
)
# Abaixo e no limiar: segmento inferior; acima: segmento superior
check_threshold <- function(fun, t, lower, upper) {
  expect_equal(fun(t - eps), lower(t - eps), tolerance = 1e-10)
  expect_equal(fun(t), lower(t), tolerance = 1e-10)
  expect_equal(fun(t + eps), upper(t + eps), tolerance = 1e-10)
}
const <- function(v) function(x) rep(v, length(x))

test_that("OD: limiares 100% e 140% de saturacao", {
  check_threshold(.qi_igam_od, 100, seg$od_le100, seg$od_100_140)
  check_threshold(.qi_igam_od, 140, seg$od_100_140, const(47))
  expect_equal(.qi_igam_od(0), seg$od_le100(0))
  expect_true(is.na(.qi_igam_od(NA_real_)))
})

test_that("Coliformes: limiares 1 e 1e5 NMP/100 mL", {
  expect_true(is.na(.qi_igam_coliformes(1 - eps)))     # fora do dominio SCQA
  expect_equal(.qi_igam_coliformes(1), seg$cf(1))
  check_threshold(.qi_igam_coliformes, 1e5, seg$cf, const(3))
})

test_that("pH: limiares 2; 6,9; 7,1; 12", {
  check_threshold(.qi_igam_ph, 2, const(2), seg$ph_a)
  check_threshold(.qi_igam_ph, 6.9, seg$ph_a, seg$ph_b)
  check_threshold(.qi_igam_ph, 7.1, seg$ph_b, seg$ph_c)
  check_threshold(.qi_igam_ph, 12, seg$ph_c, const(3))
})

test_that("DBO: limiar 30 mg/L", {
  check_threshold(.qi_igam_dbo, 30, seg$dbo, const(2))
  expect_equal(.qi_igam_dbo(0), seg$dbo(0))
})

test_that("Nitrato (mg/L NO3): limiares 10, 60 e 90", {
  check_threshold(.qi_igam_nitrato, 10, seg$no3_a, seg$no3_b)
  check_threshold(.qi_igam_nitrato, 60, seg$no3_b, seg$no3_c)
  check_threshold(.qi_igam_nitrato, 90, seg$no3_c, const(1))
})

test_that("Fosfato total, turbidez e solidos totais: limiares 10, 100 e 500", {
  check_threshold(.qi_igam_fosfato, 10, seg$po4, const(5))
  check_threshold(.qi_igam_turbidez, 100, seg$tu, const(5))
  check_threshold(.qi_igam_solidos, 500, seg$st, const(30))
  expect_equal(.qi_igam_turbidez(0), seg$tu(0))
})

# Amostras SINTETICAS (nao sao medicoes do IGAM). Valores esperados calculados
# FORA do pacote, por transcricao independente em Python
# (audit/igam_valores_esperados_sinteticos.py), e fixados aqui. Cobrem todos os
# trechos dos nove subindices: OD <= 100, 100-140 e > 140; coliformes 1 e
# > 1e5; pH <= 2, 2-6.9, 6.9-7.1 e 7.1-12; DBO > 30; nitrato nos quatro
# trechos; fosfato > 10; turbidez = 100 e > 100; ST = 500 e > 500; ausente.
test_that("API: IQA completo de amostras sinteticas = calculo independente (Python)", {
  A <- data.frame(
    od = c(7, 3, 12, 7, 20), temperatura = c(25, 20, 15, 25, 10), cloreto = c(10, 50, 5, 10, 0),
    e_coli = c(100, 2e5, 10, 100, 1), ph = c(7.3, 6.5, 8.5, 7.3, 2), dbo = c(2, 35, 10, 2, 0),
    nitrato = c(1, 5, 15, 1, 0.02), p_total = c(0.1, 0.5, 4, 0.1, 0.01),
    turbidez = c(5, 150, 50, 5, 0), solidos_totais = c(100, 600, 300, NA, 0)
  )
  oa <- iqa(A, method = "IGAM", altitude_m = c(0, 800, 300, 0, 0), nitrate_unit = "N",
            phosphorus_basis = "P", details = TRUE)
  expect_equal(attr(oa, "components")$IQA_unrounded,
               c(75.479124819573599, 17.054590358404308, 38.214773570129871,
                 74.843036542785725, 53.248447741776744), tolerance = 1e-10)
  expect_equal(oa$IQA, c(75.5, 17.1, 38.2, 74.8, 53.2))
  expect_equal(as.character(oa$IQA_status), c("Bom", "Muito Ruim", "Ruim", "Bom", "Medio"))
  expect_equal(attr(oa, "iqa_partial_rows"), 4L)     # ST ausente: pesos redistribuidos
  B <- data.frame(
    od = c(6, 9.5), temperatura = c(22, 18), cloreto = c(20, 2), coliformes = c(1000, 50),
    ph = c(7, 9.5), dbo = c(5, 1), nitrato = c(8, 95), fosfato_total = c(0.3, 11),
    turbidez = c(30, 100), solidos_totais = c(250, 500)
  )
  ob <- iqa(B, method = "IGAM", altitude_m = c(500, 1000), nitrate_unit = "NO3", details = TRUE)
  expect_equal(attr(ob, "components")$IQA_unrounded,
               c(59.080906158297132, 28.673386078401698), tolerance = 1e-10)
  expect_equal(ob$IQA, c(59.1, 28.7))
  expect_equal(as.character(ob$IQA_status), c("Medio", "Ruim"))
  expect_equal(attr(ob, "iqa_microbial_type"), "thermotolerant_coliforms")
})

base_igam <- function() {
  data.frame(od = 7, temperatura = 25, cloreto = 10, e_coli = 100, ph = 7.3, dbo = 2,
             nitrato = 1, fosfato_total = 0.1, turbidez = 5, solidos_totais = 100)
}

test_that("API: unidades de nitrato e fosforo so com declaracao explicita", {
  d <- base_igam()
  expect_error(iqa(d, method = "IGAM"), "nitrate_unit")
  a <- iqa(d, method = "IGAM", nitrate_unit = "NO3", details = TRUE)
  b <- iqa(d, method = "IGAM", nitrate_unit = "N", details = TRUE)
  expect_equal(attr(a, "components")$qi_nitrato, .qi_igam_nitrato(1))
  expect_equal(attr(b, "components")$qi_nitrato, .qi_igam_nitrato(62.004 / 14.007))
  # fosfato_total (PO4) com base "P" declarada: contradicao -> erro
  expect_error(iqa(d, method = "IGAM", nitrate_unit = "NO3", phosphorus_basis = "P"), "contradicts")
  # p_total: exige base; "P" converte, "PO4" usa direto
  dp <- d; dp$fosfato_total <- NULL; dp$p_total <- 0.1
  expect_error(iqa(dp, method = "IGAM", nitrate_unit = "NO3"), "phosphorus_basis")
  p <- iqa(dp, method = "IGAM", nitrate_unit = "NO3", phosphorus_basis = "P", details = TRUE)
  q <- iqa(dp, method = "IGAM", nitrate_unit = "NO3", phosphorus_basis = "PO4", details = TRUE)
  expect_equal(attr(p, "components")$fosfato_total_mgL_PO4, 0.1 * 94.9714 / 30.9738)
  expect_equal(attr(q, "components")$fosfato_total_mgL_PO4, 0.1)
  expect_false("fosfato_total" %in% names(q))     # df do usuario preservado
  # ortofosfato e NT nao substituem fosfato total e nitrato
  do <- d; do$fosfato_total <- NULL; do$nitrato <- NULL; do$p_ortofosfato <- 0.1; do$nt_total <- 3
  r <- iqa(do, method = "IGAM", details = TRUE)
  cp <- attr(r, "components")
  expect_true(is.na(cp$qi_fosfato_total)); expect_true(is.na(cp$qi_nitrato))
  expect_match(cp$observacao, "nitrato"); expect_match(cp$observacao, "fosfato_total")
})

test_that("API: microbiologico, ausencias essenciais e opcoes do CETESB", {
  d <- base_igam()
  both <- d; both$coliformes <- 50
  expect_error(iqa(both, method = "IGAM", nitrate_unit = "NO3"), "microbial_type")
  ec <- iqa(both, method = "IGAM", nitrate_unit = "NO3", microbial_type = "e_coli", details = TRUE)
  ct <- iqa(both, method = "IGAM", nitrate_unit = "NO3", microbial_type = "thermotolerant_coliforms", details = TRUE)
  expect_equal(attr(ec, "components")$qi_microbiologico, .qi_igam_coliformes(100))
  expect_equal(attr(ct, "components")$qi_microbiologico, .qi_igam_coliformes(50))   # sem fator 1,25
  for (col in c("od", "e_coli", "cloreto", "temperatura")) {
    x <- d; x[[col]] <- NULL
    o <- iqa(x, method = "IGAM", nitrate_unit = "NO3", details = TRUE)
    expect_true(is.na(o$IQA), info = col)
    expect_false(is.na(attr(o, "components")$observacao), info = col)
  }
  expect_error(iqa(d, method = "IGAM", nitrate_unit = "NO3", allow_partial = TRUE), "allow_partial")
  expect_error(iqa(d, method = "IGAM", nitrate_unit = "NO3", pesos = c(od = 0.2)), "pesos")
  expect_error(iqa(d, method = "IGAM", nitrate_unit = "NO3", temperature_method = "delta"), "qs = 92")
  expect_error(iqa(d, method = "IGAM", nitrate_unit = "NO3", censor_policy = "preserve"), "preserve")
})

test_that("API: censura, altitude por linha, vazio e invalidos", {
  d <- base_igam()[c(1, 1), ]
  d$dbo <- c("<2", "2"); d$e_coli <- c(">24196", "24196")
  o <- iqa(d, method = "IGAM", nitrate_unit = "NO3", altitude_m = c(0, 800), details = TRUE)
  expect_equal(attr(o, "iqa_igam_censored")$dbo, c(TRUE, FALSE))
  expect_equal(attr(o, "iqa_censor_policy"), "limit")
  cp <- attr(o, "components")
  expect_gt(cp$od_saturacao_pct[2], cp$od_saturacao_pct[1])   # altitude maior, Cs menor
  h <- iqa(d, method = "IGAM", nitrate_unit = "NO3", censor_policy = "half_limit", details = TRUE)
  expect_equal(attr(h, "components")$qi_dbo[1], .qi_igam_dbo(1))
  expect_error(iqa(d, method = "IGAM", nitrate_unit = "NO3", altitude_m = c(0, 1, 2)), "altitude_m")
  z <- iqa(base_igam()[0, ], method = "IGAM", nitrate_unit = "NO3")
  expect_equal(nrow(z), 0L)
  bad <- base_igam(); bad$ph <- 15
  expect_warning(o2 <- iqa(bad, method = "IGAM", nitrate_unit = "NO3", details = TRUE), "ph")
  expect_equal(attr(o2, "components")$w_ph, 0)                 # ausente: redistribuido
  expect_equal(attr(o2, "iqa_partial_rows"), 1L)
})

test_that("API: turbidez 0/100/100+eps e valores nao finitos; OD e microbiologico invalidos", {
  b <- base_igam()
  tu <- c(0, 100, 100 + 1e-9, -1, NA, NaN, Inf)
  d <- b[rep(1, length(tu)), ]; d$turbidez <- tu
  o <- suppressWarnings(iqa(d, method = "IGAM", nitrate_unit = "NO3", details = TRUE))
  cp <- attr(o, "components")
  expect_equal(cp$qi_turbidez[1:3], c(.qi_igam_turbidez(0), .qi_igam_turbidez(100), 5))
  # Invalidos/ausentes: turbidez nao e obrigatoria -> peso redistribuido, com motivo
  expect_true(all(is.na(cp$qi_turbidez[4:7])))
  expect_true(all(cp$w_turbidez[4:7] == 0))
  expect_true(all(grepl("turbidez", cp$observacao[4:7])))
  expect_warning(iqa(d[4, ], method = "IGAM", nitrate_unit = "NO3"), "turbidez")
  # OD/microbiologico obrigatorios: invalido -> sem IQA
  d2 <- b[rep(1, 5), ]
  d2$od <- c(NaN, Inf, -1, 7, 7); d2$e_coli <- c(100, 100, 100, NaN, Inf)
  o2 <- suppressWarnings(iqa(d2, method = "IGAM", nitrate_unit = "NO3", details = TRUE))
  expect_true(all(is.na(o2$IQA)))
  expect_true(all(!is.na(attr(o2, "components")$observacao)))
})

test_that("CETESB inalterado pela liberacao do IGAM", {
  d <- data.frame(od = 6.5, temperatura = 25, dbo = 2, coliformes = 200, ph = 7.2,
                  nt_total = 1.2, p_total = 0.05, turbidez = 8, solidos_totais = 120)
  expect_identical(iqa(d)$IQA, iqa(d, method = "CETESB")$IQA)
  # nitrate_unit nao afeta o CETESB
  expect_identical(iqa(d, nitrate_unit = "N")$IQA, iqa(d)$IQA)
  expect_equal(attr(iqa(d), "iqa_calculation_method"), "CETESB")
})

cmp_data <- function() {
  data.frame(
    ponto = c("P1", "P2", "P3", "P4"),
    od = c(7, 5, 3, 6), temperatura = 24, cloreto = c(10, 30, 60, NA),
    coliformes = c(100, 2000, 20000, 500), ph = c(7.2, 7.5, 7, 7.4),
    dbo = c(2, 5, 15, NA), nt_total = c(1, 3, 8, 2), nitrato = c(0.5, 1, 2, 0.8),
    p_total = c(0.05, 0.2, 0.8, 0.1), turbidez = c(5, 20, 60, 10),
    solidos_totais = c(100, 200, 400, 150)
  )
}

test_that("compare_iqa(): CETESB x IGAM com ambos disponiveis", {
  d <- cmp_data()
  cmp <- compare_iqa(d, id_cols = "ponto", phosphorus_basis = "P", nitrate_unit = "N",
                     altitude_m = 800, details = TRUE)
  expect_equal(cmp$ponto, d$ponto)                        # alinhamento por linha
  ig <- iqa(d, method = "IGAM", phosphorus_basis = "P", nitrate_unit = "N", altitude_m = 800, details = TRUE)
  ce <- iqa(d, phosphorus_basis = "P", altitude_m = 800, details = TRUE)
  expect_equal(cmp$IQA_IGAM, ig$IQA)
  expect_equal(cmp$IQA_CETESB, ce$IQA)
  # P4: sem cloreto (IGAM) e sem DBO (CETESB): inelegivel, com motivos
  expect_equal(cmp$par_valido, c(TRUE, TRUE, TRUE, FALSE))
  expect_match(cmp$motivo_IGAM[4], "cloreto")
  expect_match(cmp$motivo_CETESB[4], "dbo")
  dlt <- attr(ig, "components")$IQA_unrounded - attr(ce, "components")$IQA_unrounded
  expect_equal(cmp$delta_IGAM_minus_CETESB[1:3], round(dlt[1:3], 4))
  expect_true(is.na(cmp$delta_IGAM_minus_CETESB[4]))
  s <- attr(cmp, "summary")
  expect_equal(s$n_paired, 3L)
  expect_equal(s$mean_diff, mean(dlt[1:3]))
  expect_equal(s$rmse, sqrt(mean(dlt[1:3]^2)))
  tr <- attr(cmp, "class_transitions")$IGAM_minus_CETESB
  expect_equal(sum(tr$n), 3L)
  expect_true(all(c("classe_CETESB", "classe_IGAM", "n") %in% names(tr)))
  expect_named(attr(cmp, "components"), c("CETESB", "IGAM"))
  # Ordem invertida: IGAM como referencia
  inv <- compare_iqa(d, methods = c("IGAM", "CETESB"), id_cols = "ponto",
                     phosphorus_basis = "P", nitrate_unit = "N", altitude_m = 800)
  expect_equal(inv$delta_CETESB_minus_IGAM, -cmp$delta_IGAM_minus_CETESB)
  # Menos de 3 pares: correlacao indisponivel
  two <- compare_iqa(d[1:2, ], id_cols = "ponto", phosphorus_basis = "P", nitrate_unit = "N")
  expect_true(is.na(attr(two, "summary")$cor_pearson))
  one <- compare_iqa(d[1, ], id_cols = "ponto", phosphorus_basis = "P", nitrate_unit = "N")
  expect_equal(attr(one, "summary")$n_paired, 1L)
  expect_true(is.na(attr(one, "summary")$sd_diff))
  dup <- d; dup$ponto[2] <- "P1"
  expect_error(compare_iqa(dup, id_cols = "ponto", phosphorus_basis = "P", nitrate_unit = "N"), "share the same")
  expect_identical(d, cmp_data())
})

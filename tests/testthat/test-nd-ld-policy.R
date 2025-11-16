test_that(".parse_nd_ld detects and handles censored values with ld2 policy", {
  # Helper interno, precisa acessar via namespace
  parse_nd_ld <- get(".parse_nd_ld", envir = asNamespace("tikatuwq"), inherits = FALSE)
  
  # Valores censurados com valor numerico
  x <- c("<0.01", "< 0.1", "0.10", "<0.20")
  result <- parse_nd_ld(x, ld_policy = "ld2")
  
  expect_equal(result[1], 0.01 / 2)  # <0.01 -> 0.005
  expect_equal(result[2], 0.1 / 2)   # < 0.1 -> 0.05
  expect_equal(result[3], 0.10)       # Normal
  expect_equal(result[4], 0.20 / 2)  # <0.20 -> 0.10
})

test_that(".parse_nd_ld handles different policies correctly", {
  parse_nd_ld <- get(".parse_nd_ld", envir = asNamespace("tikatuwq"), inherits = FALSE)
  
  x <- c("<0.10", "<LD", "ND", "0.5")
  
  # ld2: metade do limite
  result_ld2 <- parse_nd_ld(x, ld_policy = "ld2")
  expect_equal(result_ld2[1], 0.05)
  expect_true(is.na(result_ld2[2]) || result_ld2[2] == 0)  # <LD sem valor
  expect_true(is.na(result_ld2[3]) || result_ld2[3] == 0)  # ND
  expect_equal(result_ld2[4], 0.5)
  
  # ld: limite exato
  result_ld <- parse_nd_ld(x, ld_policy = "ld")
  expect_equal(result_ld[1], 0.10)
  
  # zero: substitui por 0
  result_zero <- parse_nd_ld(x, ld_policy = "zero")
  expect_equal(result_zero[1], 0)
  expect_equal(result_zero[2], 0)
  expect_equal(result_zero[3], 0)
  expect_equal(result_zero[4], 0.5)
  
  # na: substitui por NA
  result_na <- parse_nd_ld(x, ld_policy = "na")
  expect_true(is.na(result_na[1]))
  expect_true(is.na(result_na[2]))
  expect_true(is.na(result_na[3]))
  expect_equal(result_na[4], 0.5)
})

test_that("read_wq applies nd_policy correctly", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(
    c("ponto;p_total;od",
      "P1;<0.01;7.5",
      "P2;0.10;6.8",
      "P3;<LD;8.0",
      "P4;ND;7.2"),
    tmp
  )
  
  # Testa politica padrao (ld2)
  df_ld2 <- read_wq(tmp, nd_policy = "ld2")
  expect_true(is.numeric(df_ld2$p_total))
  expect_equal(df_ld2$p_total[1], 0.005)  # <0.01 / 2
  
  # Testa politica "ld"
  df_ld <- read_wq(tmp, nd_policy = "ld")
  expect_equal(df_ld$p_total[1], 0.01)
  
  # Testa politica "zero"
  df_zero <- read_wq(tmp, nd_policy = "zero")
  expect_equal(df_zero$p_total[1], 0)
  expect_equal(df_zero$p_total[3], 0)
  expect_equal(df_zero$p_total[4], 0)
  
  unlink(tmp)
})

test_that("read_wq handles mixed censored and normal values", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(
    c("ponto;turbidez;od",
      "P1;5.0;7.5",
      "P2;<2.0;6.8",
      "P3;10.0;<5.0"),
    tmp
  )
  
  df <- read_wq(tmp, nd_policy = "ld2")
  expect_equal(df$turbidez[1], 5.0)
  expect_equal(df$turbidez[2], 1.0)  # <2.0 / 2
  expect_equal(df$turbidez[3], 10.0)
  expect_equal(df$od[1], 7.5)
  expect_equal(df$od[2], 6.8)
  expect_equal(df$od[3], 2.5)  # <5.0 / 2
  
  unlink(tmp)
})

test_that("read_wq handles NA and non-censored strings gracefully", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(
    c("ponto;p_total;od",
      "P1;<0.01;7.5",
      "P2;;6.8",
      "P3;invalid;8.0"),
    tmp
  )
  
  # Nao deve quebrar com valores invalidos
  df <- read_wq(tmp, nd_policy = "ld2")
  expect_true(is.numeric(df$p_total))
  expect_true(is.na(df$p_total[2]) || df$p_total[2] == 0)
  
  unlink(tmp)
})


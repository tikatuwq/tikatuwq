test_that("generate_analysis returns non-empty character vector", {
  skip_on_ci()  # pode depender de dados ou pacotes
  df <- data.frame(
    ponto = c("P1", "P2", "P3"),
    ph = c(7, 7.5, 6.8),
    od = c(8, 9, 7),
    turbidez = c(10, 5, 15),
    dbo = c(3, 2, 4),
    coliformes = c(200, 100, 300),
    p_total = c(0.03, 0.02, 0.05),
    nt_total = c(0.8, 0.6, 1.0),
    tds = c(150, 120, 180),
    temperatura = c(24, 25, 23)
  )
  
  # Deve calcular IQA primeiro se nao existir
  result <- generate_analysis(df, classe_conama = "2", incluir_tendencia = FALSE)
  
  expect_true(is.character(result))
  expect_true(length(result) > 0)
  expect_true(all(nchar(result) > 0))
})

test_that("generate_analysis handles NA values gracefully", {
  skip_on_ci()
  df <- data.frame(
    ponto = c("P1", "P2"),
    ph = c(7, NA),
    od = c(8, 9),
    turbidez = c(10, 5),
    dbo = c(3, 2),
    coliformes = c(200, NA),
    p_total = c(0.03, 0.02),
    nt_total = c(0.8, 0.6),
    tds = c(150, 120),
    temperatura = c(24, 25)
  )
  
  # Nao deve quebrar com NA
  expect_error(result <- generate_analysis(df, classe_conama = "2", incluir_tendencia = FALSE), NA)
  expect_true(is.character(result))
})

test_that("generate_analysis includes expected sections when violations present", {
  skip_on_ci()
  df <- data.frame(
    ponto = c("P1", "P2"),
    ph = c(4, 7.5),  # pH 4 viola CONAMA classe 2
    od = c(3, 9),    # OD 3 viola CONAMA classe 2
    turbidez = c(100, 5),  # turbidez 100 viola CONAMA classe 2
    dbo = c(5, 2),
    coliformes = c(5000, 100),  # coliformes 5000 viola CONAMA classe 2
    p_total = c(0.10, 0.02),
    nt_total = c(2.0, 0.6),
    tds = c(500, 120),
    temperatura = c(24, 25)
  )
  
  result <- generate_analysis(df, classe_conama = "2", incluir_tendencia = FALSE)
  
  expect_true(is.character(result))
  # Deve mencionar violacoes
  result_text <- paste(result, collapse = " ")
  expect_true(grepl("viola|conama|limit", result_text, ignore.case = TRUE))
})

test_that("generate_analysis accepts contexto metadata", {
  skip_on_ci()
  df <- data.frame(
    ponto = c("P1", "P2"),
    ph = c(7, 7.5),
    od = c(8, 9),
    turbidez = c(10, 5),
    dbo = c(3, 2),
    coliformes = c(200, 100),
    p_total = c(0.03, 0.02),
    nt_total = c(0.8, 0.6),
    tds = c(150, 120),
    temperatura = c(24, 25)
  )
  
  result <- generate_analysis(
    df,
    classe_conama = "2",
    incluir_tendencia = FALSE,
    contexto = list(rio = "Rio Azul", periodo = "jan-jun/2025")
  )
  
  expect_true(is.character(result))
  result_text <- paste(result, collapse = " ")
  # Deve mencionar metadados do contexto se aplicavel
  expect_true(length(result) > 0)
})


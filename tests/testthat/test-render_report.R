test_that("render_report creates output file when rmarkdown available", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_ci()  # pode ser lento
  
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
  
  # Use withr::local_tempdir() for CRAN compliance
  if (requireNamespace("withr", quietly = TRUE)) {
    tmp_dir <- withr::local_tempdir()
  } else {
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  }
  
  result_path <- render_report(df, output_dir = tmp_dir)
  
  expect_true(file.exists(result_path))
  expect_true(grepl("\\.html$", result_path))
  # Verify output is within tmp_dir
  expect_true(
    startsWith(
      normalizePath(result_path, winslash = "/", mustWork = FALSE),
      normalizePath(tmp_dir, winslash = "/", mustWork = FALSE)
    )
  )
})

test_that("render_report errors when rmarkdown not available", {
  skip_on_cran()
  skip_if(requireNamespace("rmarkdown", quietly = TRUE))
  
  df <- data.frame(ponto = "P1", ph = 7)
  
  expect_error(
    render_report(df),
    "rmarkdown"
  )
})

test_that("render_report accepts meta parameter", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_on_ci()
  
  df <- data.frame(
    ponto = "P1",
    ph = 7, od = 8, turbidez = 10, dbo = 3,
    coliformes = 200, p_total = 0.03, nt_total = 0.8,
    tds = 150, temperatura = 24
  )
  
  # Use withr::local_tempdir() for CRAN compliance
  if (requireNamespace("withr", quietly = TRUE)) {
    tmp_dir <- withr::local_tempdir()
  } else {
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  }
  
  result_path <- render_report(
    df,
    meta = list(river = "Rio Azul", period = "2025-01"),
    output_dir = tmp_dir
  )
  
  expect_true(file.exists(result_path))
  # Verify output is within tmp_dir
  expect_true(
    startsWith(
      normalizePath(result_path, winslash = "/", mustWork = FALSE),
      normalizePath(tmp_dir, winslash = "/", mustWork = FALSE)
    )
  )
})


test_that("wq_demo dataset has expected shape and columns", {
  utils::data("wq_demo", package = "tikatuwq", envir = environment())
  expect_true(exists("wq_demo"))
  expect_s3_class(wq_demo, "data.frame")
  expect_equal(nrow(wq_demo), 20)

  expected_cols <- c("ponto","data","ph","od","turbidez","dbo",
                     "coliformes","p_total","nt_total","temperatura")
  expect_true(all(expected_cols %in% names(wq_demo)))

  # Basic classes
  expect_true(is.character(wq_demo$ponto))
  expect_true(inherits(wq_demo$data, "Date"))
  expect_true(is.numeric(wq_demo$ph))
  expect_true(is.numeric(wq_demo$od))
  expect_true(is.numeric(wq_demo$turbidez))
  expect_true(is.numeric(wq_demo$dbo))
  expect_true(is.integer(wq_demo$coliformes) || is.numeric(wq_demo$coliformes))
  expect_true(is.numeric(wq_demo$p_total))
  expect_true(is.numeric(wq_demo$nt_total))
  expect_true(is.numeric(wq_demo$temperatura))
})



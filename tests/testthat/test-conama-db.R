test_that("conama_limits loader works and has schema", {
  lim <- conama_limits()
  expect_true(all(c("classe","parametro","unidade","min","max","observacao","status") %in% names(lim)))
  expect_true(nrow(lim) > 0)
})

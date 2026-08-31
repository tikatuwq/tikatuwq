test_that("plot_iet() retorna ggplot para metodo carlson", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(ponto = c("P1", "P2", "P3"),
                  IET   = c(25, 45, 65))
  p <- plot_iet(d, method = "carlson")
  expect_s3_class(p, "ggplot")
})

test_that("plot_iet() retorna ggplot para metodo lamparelli", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(ponto = c("P1", "P2", "P3"),
                  IET_Lamparelli = c(45, 55, 68))
  p <- plot_iet(d, method = "lamparelli")
  expect_s3_class(p, "ggplot")
})

test_that("plot_iet() erro quando coluna IET ausente", {
  d <- data.frame(ponto = "P1", turbidez = 10)
  expect_error(plot_iet(d), "nao detectada")
})

test_that("plot_iet() orientation=horizontal nao produz erro", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(ponto = c("P1", "P2"), IET = c(30, 55))
  expect_s3_class(plot_iet(d, orientation = "horizontal"), "ggplot")
})

test_that("plot_iet() com iet_col explicito funciona", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(ponto = c("P1", "P2"), meu_iet = c(35, 60))
  p <- plot_iet(d, iet_col = "meu_iet", method = "carlson")
  expect_s3_class(p, "ggplot")
})

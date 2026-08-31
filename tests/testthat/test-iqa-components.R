# tests/testthat/test-iqa-components.R
# Testes da camada de auditoria e consistencia matematica do IQA

test_that("Os nove pesos oficiais CETESB somam exatamente 1.0", {
  weights <- c(
    od = 0.17, coliformes = 0.15, ph = 0.12, dbo = 0.10,
    nt_total = 0.10, p_total = 0.10, temperatura = 0.10,
    turbidez = 0.08, solidos_totais = 0.08
  )
  expect_equal(sum(weights), 1.0)
  expect_length(weights, 9)
})

test_that("iqa_components() retorna todos os termos e IQA consistente com prod(Qi^Wi)", {
  d <- data.frame(
    od = 6.5, temperatura = 25, dbo = 2, coliformes = 200,
    ph = 7.2, nt_total = 1.2, p_total = 0.05, turbidez = 8,
    solidos_totais = 120
  )
  comps <- iqa_components(d)
  expect_true(is.data.frame(comps))
  expect_equal(nrow(comps), 1)

  # Verifica que IQA = prod(termos)
  terms <- c(comps$term_od, comps$term_colif, comps$term_ph, comps$term_dbo,
             comps$term_nt, comps$term_pt, comps$term_temp, comps$term_turb, comps$term_st)
  expected_iqa <- prod(terms)
  expect_equal(comps$IQA[1], expected_iqa, tolerance = 1e-6)

  # iqa() com details = TRUE anexa o atributo components
  out_det <- iqa(d, details = TRUE)
  expect_true(!is.null(attr(out_det, "components")))
})

test_that("classify_iqa() aplica as faixas oficiais CETESB por default", {
  # Pessima <= 19
  expect_equal(as.character(classify_iqa(19)), "Pessima")
  # Ruim 19-36
  expect_equal(as.character(classify_iqa(19.1)), "Ruim")
  expect_equal(as.character(classify_iqa(36)), "Ruim")
  # Regular 36-51
  expect_equal(as.character(classify_iqa(36.1)), "Regular")
  expect_equal(as.character(classify_iqa(51)), "Regular")
  # Boa 51-79
  expect_equal(as.character(classify_iqa(51.1)), "Boa")
  expect_equal(as.character(classify_iqa(79)), "Boa")
  # Otima > 79
  expect_equal(as.character(classify_iqa(79.1)), "Otima")
  expect_equal(as.character(classify_iqa(100)), "Otima")
})

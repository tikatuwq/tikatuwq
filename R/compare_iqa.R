# R/compare_iqa.R
# Comparacao pareada entre motores de calculo do IQA
# (ASCII-only no codigo)

#' Compara motores de calculo do IQA em amostras pareadas
#'
#' @description
#' Calcula o IQA da mesma tabela com cada motor independente (\code{method} de
#' \code{\link{iqa}}), mantendo a identidade de cada linha, e resume as
#' diferencas \strong{apenas nas linhas validas para todos os metodos}
#' (amostras pareadas).
#'
#' @details
#' \itemize{
#'   \item Cada metodo usa a sua propria regra oficial de completude
#'     (\code{allow_partial = FALSE}); linhas que um metodo nao calcula ficam
#'     inelegiveis, com o motivo em \code{motivo_<metodo>}.
#'   \item Os metodos usam entradas diferentes: o CETESB usa nitrogenio total
#'     e fosforo total; o IGAM usa nitrato (com \code{nitrate_unit}), fosfato
#'     total ou fosforo total com \code{phosphorus_basis} explicito, e cloreto.
#'     Informe esses argumentos em \code{...}; eles valem para ambos os
#'     metodos (o CETESB ignora \code{nitrate_unit}).
#'   \item Metodo indisponivel em \code{\link{iqa_methods}} nao gera erro:
#'     todas as linhas ficam inelegiveis para ele, com o motivo.
#'   \item As escalas de classe CETESB e IGAM tem rotulos e cortes diferentes.
#'     A funcao nao calcula "percentual de concordancia" comparando rotulos;
#'     em vez disso devolve a tabela de transicoes de classe (atributo
#'     \code{"class_transitions"}) e as diferencas numericas.
#'   \item As diferencas usam o IQA nao arredondado. Correlacoes so sao
#'     calculadas com pelo menos 3 pares; abaixo disso ficam \code{NA}.
#'   \item \code{details = TRUE} anexa os componentes (qi, pesos efetivos) de
#'     cada metodo no atributo \code{"components"}. A funcao nao atribui causa
#'     as diferencas.
#'   \item ANA nao e incluida (nao ha motor ANA distinto; ver
#'     \code{system.file("audit", "ANA_VS_CETESB.md", package = "tikatuwq")}),
#'     nem as rotas legadas/NSF, que sao aproximacoes nao validadas.
#' }
#'
#' @param df Data frame com as observacoes (nao e modificado).
#' @param methods Vetor com dois ou mais metodos distintos entre
#'   \code{"CETESB"} e \code{"IGAM"}. O primeiro e a referencia das diferencas.
#' @param id_cols Nomes de colunas que identificam cada linha (ex.:
#'   \code{c("ponto", "data")}). Devem ser unicas em conjunto. Se \code{NULL},
#'   usa o numero da linha (\code{.row}).
#' @param details Logico; anexa os componentes de cada metodo.
#' @param missing_policy Politica para linhas incompletas. Apenas
#'   \code{"paired"} (estatisticas so com linhas validas em todos os metodos).
#' @param ... Argumentos repassados a \code{\link{iqa}} para todos os metodos
#'   (ex.: \code{altitude_m}, \code{censor_policy}). \code{allow_partial},
#'   \code{classification}, \code{method}, \code{details} e \code{pesos} nao
#'   sao aceitos.
#'
#' @return Data frame com uma linha por observacao: identificadores,
#'   \code{IQA_<metodo>}, \code{classe_<metodo>}, \code{elegivel_<metodo>},
#'   \code{motivo_<metodo>}, \code{par_valido} e, para cada metodo alem do
#'   primeiro, \code{delta_<metodo>_minus_<referencia>} e
#'   \code{abs_delta_<metodo>_minus_<referencia>}. Atributos:
#'   \code{"summary"} (contagens e estatisticas pareadas), \code{"class_transitions"},
#'   \code{"methods"}, \code{"missing_policy"} e, se pedido, \code{"components"}.
#'
#' @examples
#' # Dados ilustrativos (nao sao medicoes reais) com as entradas dos dois
#' # metodos; P4 nao tem cloreto e fica inelegivel para o IGAM.
#' d <- data.frame(
#'   ponto = c("P1", "P2", "P3", "P4"),
#'   od = c(7, 5, 3, 6), temperatura = 24, cloreto = c(10, 30, 60, NA),
#'   coliformes = c(100, 2000, 20000, 500), ph = c(7.2, 7.5, 7, 7.4),
#'   dbo = c(2, 5, 15, 3), nt_total = c(1, 3, 8, 2), nitrato = c(0.5, 1, 2, 0.8),
#'   p_total = c(0.05, 0.2, 0.8, 0.1), turbidez = c(5, 20, 60, 10),
#'   solidos_totais = c(100, 200, 400, 150)
#' )
#' cmp <- compare_iqa(d, id_cols = "ponto", phosphorus_basis = "P",
#'                    nitrate_unit = "N", altitude_m = 800)
#' cmp[, c("ponto", "IQA_CETESB", "IQA_IGAM", "par_valido", "motivo_IGAM")]
#' attr(cmp, "summary")
#' attr(cmp, "class_transitions")
#'
#' @seealso \code{\link{iqa}}, \code{\link{iqa_methods}}, \code{\link{classify_iqa}}
#' @export
compare_iqa <- function(
  df,
  methods = c("CETESB", "IGAM"),
  id_cols = NULL,
  details = FALSE,
  missing_policy = c("paired"),
  ...
) {
  if (!is.data.frame(df)) stop("'df' must be a data frame.")
  missing_policy <- match.arg(missing_policy)
  valid <- c("CETESB", "IGAM")
  if (!is.character(methods) || length(methods) < 2L || anyDuplicated(methods)) {
    stop("'methods' must have two or more distinct methods.")
  }
  bad <- setdiff(methods, valid)
  if (length(bad)) {
    stop("Method(s) not comparable: ", paste(bad, collapse = ", "),
         ". compare_iqa() accepts: ", paste(valid, collapse = ", "),
         ". Legacy/NSF routes are approximations and ANA has no distinct engine.")
  }
  dots <- list(...)
  forbidden <- intersect(names(dots), c("allow_partial", "classification", "method", "details", "pesos", "na_rm", "scheme"))
  if (length(forbidden)) {
    stop("Argument(s) not allowed in compare_iqa(): ", paste(forbidden, collapse = ", "),
         ". Each method uses its own official completeness rule and scale.")
  }

  ids <- .compare_ids(df, id_cols)
  res <- lapply(methods, function(m) .compare_run_method(df, m, dots))
  names(res) <- methods
  out <- .compare_assemble(ids, res)
  if (isTRUE(details)) {
    attr(out, "components") <- lapply(res, `[[`, "components")
  }
  attr(out, "missing_policy") <- missing_policy
  out
}

# Identificadores estaveis e unicos
.compare_ids <- function(df, id_cols) {
  n <- nrow(df)
  if (is.null(id_cols)) return(data.frame(.row = seq_len(n)))
  miss <- setdiff(id_cols, names(df))
  if (length(miss)) stop("id_cols not found in df: ", paste(miss, collapse = ", "))
  ids <- as.data.frame(df[, id_cols, drop = FALSE])
  dup <- duplicated(ids) | duplicated(ids, fromLast = TRUE)
  if (any(dup)) {
    stop(sum(dup), " row(s) share the same id_cols values; ",
         "add columns to id_cols (e.g. date/time) or use id_cols = NULL.")
  }
  ids
}

# Executa um metodo e devolve vetores alinhados as linhas de df
.compare_run_method <- function(df, method, dots) {
  n <- nrow(df)
  r <- tryCatch(
    do.call(iqa, c(list(df = df, method = method, details = TRUE, add_status = TRUE), dots)),
    tikatuwq_method_unavailable = function(e) e
  )
  if (inherits(r, "tikatuwq_method_unavailable")) {
    return(list(IQA = rep(NA_real_, n), IQA_raw = rep(NA_real_, n),
                classe = rep(NA_character_, n), elegivel = rep(FALSE, n),
                motivo = rep(paste0("metodo indisponivel: ", conditionMessage(r)), n),
                components = NULL, available = FALSE))
  }
  cp <- attr(r, "components")
  raw <- if (is.null(cp) || n == 0L) rep(NA_real_, n) else cp$IQA_unrounded
  ok <- !is.na(raw)
  motivo <- rep(NA_character_, n)
  if (n > 0L && any(!ok)) {
    mp <- if (!is.null(cp$missing_params)) cp$missing_params else rep(NA_character_, n)
    motivo[!ok] <- ifelse(is.na(mp[!ok]), "valores invalidos ou ausentes",
                          paste0("parametros ausentes ou invalidos: ", gsub(";", ", ", mp[!ok])))
  }
  list(IQA = r$IQA, IQA_raw = raw,
       classe = if (n > 0L) as.character(r$IQA_status) else character(0),
       elegivel = ok, motivo = motivo, components = cp, available = TRUE)
}

# Monta a tabela pareada e as estatisticas (separado para teste unitario)
.compare_assemble <- function(ids, res) {
  methods <- names(res)
  out <- ids
  for (m in methods) {
    out[[paste0("IQA_", m)]]      <- res[[m]]$IQA
    out[[paste0("classe_", m)]]   <- res[[m]]$classe
    out[[paste0("elegivel_", m)]] <- res[[m]]$elegivel
    out[[paste0("motivo_", m)]]   <- res[[m]]$motivo
  }
  paired <- Reduce(`&`, lapply(res, `[[`, "elegivel"), rep(TRUE, nrow(ids)))
  out$par_valido <- paired
  ref <- methods[1]
  stats <- list()
  trans <- list()
  for (m in methods[-1]) {
    d <- res[[m]]$IQA_raw - res[[ref]]$IQA_raw
    d[!paired] <- NA_real_
    nm <- paste0(m, "_minus_", ref)
    out[[paste0("delta_", nm)]]     <- round(d, 4)
    out[[paste0("abs_delta_", nm)]] <- round(abs(d), 4)
    stats[[nm]] <- .compare_stats(res[[ref]]$IQA_raw[paired], res[[m]]$IQA_raw[paired])
    if (any(paired)) {
      tb <- as.data.frame(table(res[[ref]]$classe[paired], res[[m]]$classe[paired]), stringsAsFactors = FALSE)
      names(tb) <- c(paste0("classe_", ref), paste0("classe_", m), "n")
      trans[[nm]] <- tb[tb$n > 0, , drop = FALSE]
    } else {
      trans[[nm]] <- data.frame()
    }
  }
  n_total <- nrow(ids)
  counts <- vapply(res, function(r) sum(r$elegivel), integer(1))
  summ <- data.frame(
    comparacao = names(stats),
    n_total = n_total,
    n_referencia = counts[[ref]],
    n_metodo = vapply(methods[-1], function(m) counts[[m]], integer(1)),
    n_paired = sum(paired),
    prop_paired = if (n_total > 0) sum(paired) / n_total else NA_real_,
    do.call(rbind, stats),
    row.names = NULL, stringsAsFactors = FALSE
  )
  attr(out, "summary") <- summ
  attr(out, "class_transitions") <- trans
  attr(out, "methods") <- methods
  attr(out, "methods_available") <- vapply(res, `[[`, logical(1), "available")
  out
}

# Estatisticas da diferenca y - x em pares validos
.compare_stats <- function(x, y) {
  n <- length(x)
  d <- y - x
  na <- NA_real_
  cr <- function(meth) if (n >= 3 && stats::sd(x) > 0 && stats::sd(y) > 0) stats::cor(x, y, method = meth) else na
  data.frame(
    mean_diff = if (n) mean(d) else na,
    median_diff = if (n) stats::median(d) else na,
    sd_diff = if (n >= 2) stats::sd(d) else na,
    mae = if (n) mean(abs(d)) else na,
    rmse = if (n) sqrt(mean(d^2)) else na,
    min_diff = if (n) min(d) else na,
    max_diff = if (n) max(d) else na,
    cor_pearson = cr("pearson"),
    cor_spearman = cr("spearman")
  )
}

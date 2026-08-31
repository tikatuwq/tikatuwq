# R/balnear.R
# Avaliacao de balneabilidade — Resolucao CONAMA 274/2000
# ASCII-only no codigo

# Limites internos por indicador (aguias doces — recreacao contato primario)
# Fonte: CONAMA 274/2000, Tabela I
.balnear_limits <- list(
  coliformes = list(  # coliformes termotolerantes (NMP/100mL)
    excelente    = 250,
    muito_boa    = 500,
    satisfatoria = 1000
  ),
  e_coli = list(      # E. coli (NMP/100mL)
    excelente    = 200,
    muito_boa    = 400,
    satisfatoria = 800
  )
)

#' Avaliacao de balneabilidade — CONAMA 274/2000
#'
#' @description
#' Classifica pontos de amostragem quanto a balneabilidade para
#' recreacao de contato primario (banho, natacao, mergulho), conforme
#' a Resolucao CONAMA 274/2000. A classificacao e baseada na regra
#' dos 80%: um ponto e considerado proprio em determinada categoria
#' quando pelo menos 80% das ultimas \code{n_samples} amostras estao
#' dentro do limite correspondente.
#'
#' @details
#' Categorias (aguas doces, contato primario):
#' \describe{
#'   \item{Excelente}{Colif. termotolerantes \eqn{\le} 250 NMP/100mL
#'     (ou E. coli \eqn{\le} 200) em \eqn{\ge} 80% das amostras.}
#'   \item{Muito Boa}{Colif. \eqn{\le} 500 (ou E. coli \eqn{\le} 400)
#'     em \eqn{\ge} 80%.}
#'   \item{Satisfatoria}{Colif. \eqn{\le} 1000 (ou E. coli \eqn{\le} 800)
#'     em \eqn{\ge} 80%.}
#'   \item{Impropria}{Mais de 20% das amostras ultrapassam o limite
#'     de "Satisfatoria".}
#' }
#'
#' A avaliacao usa as \code{n_samples} amostras mais recentes por grupo.
#' Se o grupo tiver menos amostras, a classificacao e realizada mesmo
#' assim mas a coluna \code{amostras_insuficientes} sera \code{TRUE}.
#' Recomenda-se ao menos 5 amostras (referencia: "ultimas 5 semanas"
#' do texto da norma).
#'
#' @param df Data frame com ao menos a coluna do indicador microbiologico
#'   e as colunas de agrupamento.
#' @param col Character; nome da coluna do indicador. Se \code{NULL}
#'   (default), detectada automaticamente a partir de nomes comuns:
#'   \code{coliformes}, \code{col_termotolerantes}, \code{e_coli},
#'   \code{ecoli}.
#' @param by Character vector; colunas de agrupamento. Default
#'   \code{"ponto"}.
#' @param date_col Character; nome da coluna de datas, usada para
#'   selecionar as \code{n_samples} amostras mais recentes.
#'   Default \code{"data"}.
#' @param n_samples Integer; numero de amostras mais recentes a
#'   considerar. Default \code{5L} (referencia CONAMA 274/2000).
#' @param threshold_pct Numeric; fracao minima de conformidade para
#'   classificar como "propria". Default \code{0.80} (80%).
#' @param locale Character; idioma dos rotulos:
#'   \code{"pt"} (default) ou \code{"en"}.
#'
#' @returns
#' Um tibble com uma linha por grupo, contendo:
#' \describe{
#'   \item{indicador}{Coluna usada para a avaliacao.}
#'   \item{n_amostras}{Total de amostras disponiveis no grupo.}
#'   \item{n_avaliadas}{Amostras usadas na avaliacao
#'     (\code{min(n_amostras, n_samples)}).}
#'   \item{pct_ok_satisfatoria}{Fracao dentro do limite "Satisfatoria".}
#'   \item{classificacao}{Uma de: "Excelente", "Muito Boa",
#'     "Satisfatoria", "Impropria".}
#'   \item{propria}{Logical; \code{TRUE} para as tres primeiras categorias.}
#'   \item{amostras_insuficientes}{Logical; \code{TRUE} quando
#'     \code{n_amostras < n_samples}.}
#' }
#'
#' @references
#' CONAMA (2000). Resolucao 274, de 29 de novembro de 2000. Ministerio
#' do Meio Ambiente, Brasilia. Diario Oficial da Uniao 18/01/2001.
#'
#' @seealso \code{\link[=conama_check]{conama_check()}},
#'   \code{\link[=conama_freq_check]{conama_freq_check()}}
#'
#' @family conama-tools
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#'
#' @examples
#' data("wq_demo", package = "tikatuwq")
#' balnear_check(wq_demo, by = "ponto")
#'
#' @export
balnear_check <- function(
  df,
  col           = NULL,
  by            = "ponto",
  date_col      = "data",
  n_samples     = 5L,
  threshold_pct = 0.80,
  locale        = c("pt", "en")
) {
  locale    <- match.arg(locale)
  n_samples <- as.integer(n_samples)
  stopifnot(is.data.frame(df))
  stopifnot(threshold_pct > 0 & threshold_pct <= 1)

  # Detectar coluna do indicador
  if (is.null(col)) {
    candidates <- c("coliformes", "col_termotolerantes",
                    "e_coli", "ecoli", "E_coli",
                    "coliformes_termotolerantes")
    col <- candidates[candidates %in% names(df)][1]
    if (is.na(col))
      stop("Coluna de indicador microbiologico nao detectada. ",
           "Fornecer 'col' explicitamente.")
  }
  if (!col %in% names(df))
    stop("Coluna '", col, "' nao encontrada.")

  # Definir tipo de indicador e limites
  if (grepl("e_coli|ecoli|E_coli", col, ignore.case = FALSE)) {
    lims <- .balnear_limits$e_coli
    tipo <- "e_coli"
  } else {
    lims <- .balnear_limits$coliformes
    tipo <- "coliformes"
  }

  # Grupos de agrupamento
  by_ok <- intersect(by, names(df))
  if (!length(by_ok)) {
    df[[".__grp__"]] <- "all"
    by_ok <- ".__grp__"
  }

  # Ordenar por data para "ultimas n_samples amostras"
  if (date_col %in% names(df)) {
    dates    <- as.Date(df[[date_col]])
    date_ord <- order(dates, na.last = TRUE)
    df       <- df[date_ord, , drop = FALSE]
  }

  vals <- suppressWarnings(as.numeric(df[[col]]))
  grps <- split(seq_len(nrow(df)), df[by_ok], drop = TRUE)

  # Rotulos por locale
  if (locale == "pt") {
    lbl <- c(excelente = "Excelente", muito_boa = "Muito Boa",
             satisfatoria = "Satisfatoria", impropria = "Impropria")
  } else {
    lbl <- c(excelente = "Excellent", muito_boa = "Very Good",
             satisfatoria = "Satisfactory", impropria = "Unsuitable")
  }

  result_list <- lapply(grps, function(idx) {
    row_ids    <- as.list(df[idx[1], by_ok, drop = FALSE])
    v_all      <- vals[idx]
    v_all      <- v_all[!is.na(v_all)]
    n_total    <- length(v_all)

    # Usar as n_samples amostras mais recentes (ja ordenadas por data)
    n_eval     <- min(n_total, n_samples)
    v_eval     <- if (n_total > 0) tail(v_all, n_eval) else numeric(0)

    # Fracao dentro de cada limite
    pct_exc  <- if (n_eval > 0) mean(v_eval <= lims$excelente,    na.rm = TRUE) else NA_real_
    pct_mboa <- if (n_eval > 0) mean(v_eval <= lims$muito_boa,    na.rm = TRUE) else NA_real_
    pct_sat  <- if (n_eval > 0) mean(v_eval <= lims$satisfatoria, na.rm = TRUE) else NA_real_

    # Classificacao: mais restrita que atende threshold_pct
    classe <- if (is.na(pct_sat)) {
      NA_character_
    } else if (pct_sat < threshold_pct) {
      lbl["impropria"]
    } else if (pct_mboa >= threshold_pct && pct_exc >= threshold_pct) {
      lbl["excelente"]
    } else if (pct_mboa >= threshold_pct) {
      lbl["muito_boa"]
    } else {
      lbl["satisfatoria"]
    }

    propria <- if (is.na(classe)) NA else !identical(classe, lbl["impropria"])

    base <- as.data.frame(row_ids, stringsAsFactors = FALSE)
    base[["indicador"]]             <- col
    base[["n_amostras"]]            <- n_total
    base[["n_avaliadas"]]           <- n_eval
    base[["pct_ok_satisfatoria"]]   <- if (n_eval > 0) round(pct_sat, 3) else NA_real_
    base[["classificacao"]]         <- unname(classe)
    base[["propria"]]               <- unname(propria)
    base[["amostras_insuficientes"]] <- n_total < n_samples
    base
  })

  out <- tibble::as_tibble(dplyr::bind_rows(result_list))
  out[[".__grp__"]] <- NULL
  out
}

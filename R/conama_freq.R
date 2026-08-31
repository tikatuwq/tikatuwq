# R/conama_freq.R
# Conformidade CONAMA 357/2005 baseada em frequencia (regra dos 80%/>=6 amostras)
# (ASCII-only no codigo)

#' Conformidade CONAMA 357/2005 por frequencia
#'
#' @description
#' Avalia a conformidade com a Resolucao CONAMA 357/2005 pela regra de
#' **frequencia**: um parametro e considerado conforme quando o limite e
#' atendido em pelo menos \code{threshold} das amostras (padrao 80%).
#' A regra e aplicada apenas quando ha \code{min_n} ou mais amostras no
#' grupo (padrao 6), conforme Art. 15 da Resolucao.
#'
#' @details
#' A verificacao linha-a-linha (\code{conama_check()}) nao reflete o criterio
#' estatistico da norma. Esta funcao agrupa as amostras por \code{by} e
#' por ano (extraido de \code{date_col}) e aplica a regra de frequencia.
#'
#' Quando \code{n < min_n}, a coluna \code{freq_conforme} retorna \code{NA}
#' e \code{aplicou_regra} retorna \code{FALSE} — indicando que nao ha
#' amostras suficientes para o criterio estatistico.
#'
#' @param df Data frame com colunas de parametros e ao menos as colunas
#'   indicadas em \code{by} e \code{date_col}.
#' @param classe Character; classe CONAMA 357/2005 (ex.: \code{"2"}).
#' @param by Character vector; colunas de agrupamento (ex.: \code{c("ponto","rio")}).
#'   Pode incluir qualquer coluna categorica do data frame.
#' @param date_col Character; nome da coluna de datas usada para extrair o ano.
#'   Default \code{"data"}.
#' @param min_n Integer; numero minimo de amostras por grupo para aplicar a
#'   regra de frequencia. Default \code{6}.
#' @param threshold Numeric em (0, 1]; fracao minima de conformidade exigida.
#'   Default \code{0.80} (80%).
#'
#' @returns
#' Um tibble com uma linha por combinacao \code{by + ano + parametro},
#' contendo as colunas:
#' \describe{
#'   \item{ano}{Ano extraido de \code{date_col}.}
#'   \item{parametro}{Nome do parametro avaliado.}
#'   \item{n}{Total de amostras no grupo.}
#'   \item{n_ok}{Amostras dentro do limite.}
#'   \item{pct_ok}{Percentual de conformidade (0-100).}
#'   \item{freq_conforme}{Logical; \code{TRUE} se \code{pct_ok >= threshold*100}.
#'     \code{NA} se \code{n < min_n}.}
#'   \item{aplicou_regra}{Logical; \code{TRUE} se \code{n >= min_n}.}
#' }
#'
#' @seealso \code{\link[=conama_check]{conama_check()}},
#'   \code{\link[=conama_report]{conama_report()}}
#'
#' @family conama-tools
#'
#' @examples
#' \donttest{
#' data("wq_demo", package = "tikatuwq")
#' # Conformidade por ponto e ano (regra dos 80%, min 3 amostras para este dataset)
#' conama_freq_check(wq_demo, classe = "2", min_n = 3)
#' }
#'
#' @importFrom dplyr group_by summarise across all_of n filter bind_rows
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export
conama_freq_check <- function(
  df,
  classe    = "2",
  by        = "ponto",
  date_col  = "data",
  min_n     = 6L,
  threshold = 0.80
) {
  stopifnot(is.data.frame(df))
  stopifnot(is.numeric(threshold), threshold > 0, threshold <= 1)
  min_n <- as.integer(min_n)

  # Extrai ano
  if (!date_col %in% names(df)) {
    warning("Coluna '", date_col, "' nao encontrada; agrupando sem filtro de ano.")
    df[[".__ano__"]] <- NA_integer_
  } else {
    df[[".__ano__"]] <- as.integer(format(as.Date(df[[date_col]]), "%Y"))
  }

  # Garante grupos presentes no df
  by_ok <- intersect(by, names(df))
  if (!length(by_ok)) stop("Nenhuma das colunas de agrupamento foi encontrada no data frame.")

  group_cols <- c(by_ok, ".__ano__")

  # Limites CONAMA para a classe
  lim <- conama_limits(class = classe)
  if (!nrow(lim)) {
    warning("Nenhum limite encontrado para a classe '", classe, "'.")
    return(tibble::tibble())
  }
  params_present <- intersect(unique(lim$parametro), names(df))
  if (!length(params_present)) {
    warning("Nenhum parametro CONAMA disponivel no data frame.")
    return(tibble::tibble())
  }

  # Funcao auxiliar: verifica conformidade linha-a-linha para um parametro
  .is_ok <- function(vals, lim_rows) {
    vapply(vals, function(v) {
      if (is.na(v)) return(NA)
      any(vapply(seq_len(nrow(lim_rows)), function(j) {
        mm   <- .get_minmax(lim_rows[j, , drop = FALSE])
        minv <- mm$min
        maxv <- mm$max
        (is.na(minv) | (!is.na(v) & v >= minv)) &
        (is.na(maxv) | (!is.na(v) & v <= maxv))
      }, logical(1)))
    }, logical(1))
  }

  # Processa cada parametro
  result_list <- lapply(params_present, function(p) {
    vals     <- suppressWarnings(as.numeric(df[[p]]))
    lim_rows <- lim[lim$parametro == p, , drop = FALSE]
    ok_vec   <- .is_ok(vals, lim_rows)

    tmp <- df[, group_cols, drop = FALSE]
    tmp[[".__ok__"]] <- ok_vec

    # Agrupa e sumariza
    grp <- split(tmp, tmp[group_cols], drop = TRUE)
    out_rows <- lapply(grp, function(g) {
      row_vals  <- as.list(g[1, by_ok, drop = FALSE])
      ano_val   <- g[[".__ano__"]][1]
      ok_vals   <- g[[".__ok__"]]
      n_total   <- length(ok_vals)
      n_valid   <- sum(!is.na(ok_vals))
      n_ok_val  <- sum(ok_vals, na.rm = TRUE)
      pct       <- if (n_valid > 0) round(100 * n_ok_val / n_valid, 1) else NA_real_
      aplic     <- n_valid >= min_n
      freq_conf <- if (!aplic) NA else pct >= (threshold * 100)

      base <- as.data.frame(row_vals, stringsAsFactors = FALSE)
      base[["ano"]]            <- ano_val
      base[["parametro"]]      <- p
      base[["n"]]              <- n_total
      base[["n_ok"]]           <- n_ok_val
      base[["pct_ok"]]         <- pct
      base[["freq_conforme"]]  <- freq_conf
      base[["aplicou_regra"]]  <- aplic
      base
    })
    dplyr::bind_rows(out_rows)
  })

  out <- dplyr::bind_rows(result_list)
  # Remove coluna auxiliar se existir
  out[[".__ano__"]] <- NULL

  # Reordena colunas: by_ok primeiro, depois ano, parametro, metricas
  metric_cols <- c("ano","parametro","n","n_ok","pct_ok","freq_conforme","aplicou_regra")
  col_order   <- c(by_ok, metric_cols[metric_cols %in% names(out)])
  out         <- out[, col_order, drop = FALSE]

  tibble::as_tibble(out)
}

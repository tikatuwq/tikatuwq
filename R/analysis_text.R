#' Generate analytical paragraphs (rule-based)
#'
#' @description
#' Produz 3–5 paragrafos curtos, legiveis por humanos, resumindo a qualidade da
#' agua a partir de IQA/WQI, conformidade com a CONAMA 357/2005 e (opcionalmente)
#' tendencias temporais simples. E **rule-based** (nao usa IA) e aceita metadados
#' opcionais para compor o texto.
#'
#' @param df Data frame contendo ao menos a coluna `ponto`. Recomenda-se
#'   tambem as colunas necessarias para checagens CONAMA e para o calculo do IQA.
#' @param classe_conama Character (ex. "2"). Classe-alvo para a checagem da
#'   Resolucao CONAMA 357/2005.
#' @param incluir_tendencia Logical; se `TRUE`, calcula tendencias lineares
#'   simples ao longo do tempo.
#' @param parametros_tendencia Character vector; nomes dos parametros para testar
#'   tendencia temporal.
#' @param contexto Lista com metadados opcionais (PT/EN), por exemplo
#'   `list(rio = "Rio Pardo", periodo = "jan–jun/2025", cidade = "Lencois")`.
#'   As chaves aceitas sao `rio`/`river`, `periodo`/`period`, `cidade`.
#'
#' @return Vetor `character` com 3 a 5 paragrafos analiticos prontos para relatorio.
#'
#' @examples
#' \dontrun{
#' library(tikatuwq)
#' data("wq_demo")
#' txt <- generate_analysis(
#'   df = wq_demo,
#'   classe_conama = "2",
#'   incluir_tendencia = TRUE,
#'   parametros_tendencia = c("turbidez","od","pH"),
#'   contexto = list(rio = "Rio Azul", periodo = "jan–jun/2025")
#' )
#' cat(paste(txt, collapse = "\n\n"))
#' }
#'
#' @seealso \code{\link[=iqa]{iqa()}}, \code{\link[=conama_check]{conama_check()}}
#' @family reporting-tools
#' @export
generate_analysis <- function(
  df,
  classe_conama = "2",
  incluir_tendencia = TRUE,
  parametros_tendencia = c("turbidez","od","pH"),
  contexto = list(rio = NA, periodo = NA, cidade = NA)
) {
  stopifnot("ponto" %in% names(df))

  # Garante IQA
  if (!"IQA" %in% names(df)) {
    df <- tikatuwq::iqa(df, na_rm = TRUE)
  }

  # Checagem CONAMA
  conf <- tikatuwq::conama_check(df, classe = classe_conama)

  # Helpers numericos seguros
  safe_mean <- function(x) mean(x, na.rm = TRUE)
  safe_min  <- function(x) suppressWarnings(min(x, na.rm = TRUE))
  safe_max  <- function(x) suppressWarnings(max(x, na.rm = TRUE))

  # Classificacao textual do IQA (exemplo simples em EN)
  class_iqa <- function(x){
    cut(x, breaks = c(-Inf,25,50,70,90,Inf),
        labels = c("very low","low","medium","good","excellent"),
        right = TRUE)
  }

  iqa_mean <- safe_mean(conf$IQA)
  iqa_min  <- safe_min(conf$IQA)
  iqa_max  <- safe_max(conf$IQA)
  dom_class <- names(sort(table(class_iqa(conf$IQA)), decreasing = TRUE))[1]

  by_point <- conf |>
    dplyr::group_by(.data$ponto) |>
    dplyr::summarise(IQA_med = mean(.data$IQA, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$IQA_med))

  best <- by_point$ponto[1]; best_val <- by_point$IQA_med[1]
  worst <- by_point$ponto[nrow(by_point)]; worst_val <- by_point$IQA_med[nrow(by_point)]

  # Metadados (aceita PT/EN)
  .get_meta <- function(lst, key_pt, key_en) {
    if (is.null(lst)) return(NA_character_)
    v <- lst[[key_pt]]
    if (is.null(v)) v <- lst[[key_en]]
    if (is.null(v) || length(v) == 0) return(NA_character_)
    v
  }
  river  <- .get_meta(contexto, "rio", "river")
  period <- .get_meta(contexto, "periodo", "period")

  river_txt  <- if (!is.na(river))  glue::glue("for the {river} river ") else ""
  period_txt <- if (!is.na(period)) glue::glue("in {period} ") else ""

  p1 <- glue::glue(
    "Assessing the Water Quality Index (IQA) {river_txt}{period_txt}we observed an average of {scales::number(iqa_mean, accuracy = 0.1)}, ranging from {scales::number(iqa_min, accuracy = 0.1)} to {scales::number(iqa_max, accuracy = 0.1)}. The dominant class was {dom_class}. The best-performing point was {best} (IQA~{scales::number(best_val, accuracy = 0.1)}), whereas {worst} presented the lowest value (IQA~{scales::number(worst_val, accuracy = 0.1)})."
  )

  # Violacoes CONAMA
  ok_cols <- grep("_ok$", names(conf), value = TRUE)
  long_ok <- conf |>
    tidyr::pivot_longer(dplyr::all_of(ok_cols), names_to = "param_ok", values_to = "ok") |>
    dplyr::mutate(parametro = gsub("_ok$", "", .data$param_ok))

  viol <- long_ok |>
    dplyr::filter(!.data$ok) |>
    dplyr::count(.data$ponto, .data$parametro, name = "n_viol")

  top_viol_param <- if (nrow(viol)) viol |>
    dplyr::count(.data$parametro, wt = .data$n_viol, name = "total") |>
    dplyr::arrange(dplyr::desc(.data$total)) |>
    dplyr::slice(1) else NULL

  top_viol_point <- if (nrow(viol)) viol |>
    dplyr::count(.data$ponto, wt = .data$n_viol, name = "total") |>
    dplyr::arrange(dplyr::desc(.data$total)) |>
    dplyr::slice(1) else NULL

  if (nrow(viol)) {
    p2 <- glue::glue(
      "Regarding compliance with CONAMA 357/2005 (class {classe_conama}), violations were recorded, with {top_viol_param$parametro[1]} as the most critical parameter and point {top_viol_point$ponto[1]} concentrating the highest number of occurrences."
    )
  } else {
    p2 <- glue::glue(
      "All evaluated records met the limits established by CONAMA 357/2005 (class {classe_conama}) for the checked parameters."
    )
  }

  # Tendencias (opcional)
  p3 <- NULL
  if (incluir_tendencia && "data" %in% names(conf)) {
    tend <- calc_trends(conf, parametros = intersect(parametros_tendencia, names(conf)))
    if (nrow(tend)) {
      top <- tend |>
        dplyr::arrange(dplyr::desc(abs(.data$beta))) |>
        dplyr::slice(1:min(2, dplyr::n()))
      itens <- apply(top, 1, function(r){
        dir <- ifelse(as.numeric(r["beta"]) > 0, "increasing trend", "decreasing trend")
        sig <- ifelse(as.numeric(r["p_value"]) < 0.05, " (significant, p<0.05)", "")
        glue::glue("{r['parametro']} em {r['ponto']}: {dir}{sig}")
      })
      p3 <- glue::glue("Temporal analysis indicated {paste(itens, collapse='; ')}.")
    }
  }

  # Variacao espacial de IQA
  range_iqa <- iqa_max - iqa_min
  p4 <- glue::glue(
    "The spatial variation of IQA was approximately {scales::number(range_iqa, accuracy = 0.1)} points between extremes."
  )

  # Recomendacoes
  recs <- character(0)
  if (nrow(viol)) {
    if ("coliformes_ok" %in% ok_cols && any(conf$coliformes_ok == FALSE, na.rm = TRUE)) {
      recs <- c(recs, "reinforce control of diffuse fecal sources and local sanitation;")
    }
    if ("od_ok" %in% ok_cols && any(conf$od_ok == FALSE, na.rm = TRUE)) {
      recs <- c(recs, "assess organic load/BOD and re-aeration in affected stretches;")
    }
    if ("turbidez_ok" %in% ok_cols && any(conf$turbidez_ok == FALSE, na.rm = TRUE)) {
      recs <- c(recs, "inspect bank erosion and soil management in the basin;")
    }
  }
  p5 <- if (length(recs))
    glue::glue("It is recommended to {paste(recs, collapse = ' ')} prioritizing critical points.")
  else NULL

  purrr::compact(c(p1, p2, p3, p4, p5))
}

#' @keywords internal
calc_trends <- function(df, parametros){
  if (!length(parametros)) return(dplyr::tibble())
  out <- list()
  for (p in parametros) {
    if (!p %in% names(df)) next
    tmp <- df |>
      dplyr::filter(!is.na(.data[[p]]), !is.na(.data$data)) |>
      dplyr::group_by(.data$ponto) |>
      dplyr::group_map(~{
        if (nrow(.x) < 4) return(NULL)
        tnum <- as.numeric(.x$data) # dias
        fit <- stats::lm(.x[[p]] ~ tnum)
        s <- broom::tidy(fit)
        beta <- s$estimate[s$term == "tnum"]
        pval <- s$p.value[s$term == "tnum"]
        dplyr::tibble(ponto = .x$ponto[1], parametro = p, beta = beta, p_value = pval)
      }) |>
      dplyr::bind_rows()
    out[[p]] <- tmp
  }
  dplyr::bind_rows(out)
}

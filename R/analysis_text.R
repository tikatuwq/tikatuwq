#' GeraÃ§Ã£o de parÃ¡grafos analÃ­ticos (sem IA)
#' @export
generate_analysis <- function(df,
                              classe_conama = "2",
                              incluir_tendencia = TRUE,
                              parametros_tendencia = c("turbidez","od","pH"),
                              contexto = list(rio = NA, periodo = NA, cidade = NA)) {
  stopifnot("ponto" %in% names(df))
  if(!"IQA" %in% names(df)) {
    df <- tikatuwq::iqa(df, na_rm = TRUE)
  }
  conf <- tikatuwq::conama_check(df, classe = classe_conama)

  safe_mean <- function(x) mean(x, na.rm = TRUE)
  safe_min  <- function(x) suppressWarnings(min(x, na.rm = TRUE))
  safe_max  <- function(x) suppressWarnings(max(x, na.rm = TRUE))

  class_iqa <- function(x){
    cut(x, breaks = c(-Inf,25,50,70,90,Inf),
        labels = c("muito baixa","baixa","mÃ©dia","boa","excelente"), right = TRUE)
  }

  iqa_mean <- safe_mean(conf$IQA)
  iqa_min  <- safe_min(conf$IQA)
  iqa_max  <- safe_max(conf$IQA)
  dom_class <- names(sort(table(class_iqa(conf$IQA)), decreasing = TRUE))[1]

  by_point <- conf |>
    dplyr::group_by(ponto) |>
    dplyr::summarise(IQA_med = mean(IQA, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(IQA_med))

  best <- by_point$ponto[1]; best_val <- by_point$IQA_med[1]
  worst <- by_point$ponto[nrow(by_point)]; worst_val <- by_point$IQA_med[nrow(by_point)]

  p1 <- glue::glue(
    "Avaliando o Ãndice de Qualidade da Ãgua (IQA) {if (!is.na(contexto$rio)) glue::glue('no rio {contexto$rio} ') else ''}{if (!is.na(contexto$periodo)) glue::glue('em {contexto$periodo} ') else ''}observou-se mÃ©dia de {scales::number(iqa_mean, accuracy = 0.1)}, variando de {scales::number(iqa_min, accuracy = 0.1)} a {scales::number(iqa_max, accuracy = 0.1)}. A classe dominante foi {dom_class}. O ponto com melhor desempenho foi {best} (IQAâ‰ˆ{scales::number(best_val, accuracy = 0.1)}), enquanto {worst} apresentou o menor valor (IQAâ‰ˆ{scales::number(worst_val, accuracy = 0.1)})."
  )

  ok_cols <- grep("_ok$", names(conf), value = TRUE)
  long_ok <- conf |>
    tidyr::pivot_longer(dplyr::all_of(ok_cols), names_to = "param_ok", values_to = "ok") |>
    dplyr::mutate(parametro = gsub("_ok$", "", param_ok))

  viol <- long_ok |>
    dplyr::filter(!ok) |>
    dplyr::count(ponto, parametro, name = "n_viol")

  top_viol_param <- if (nrow(viol)) viol |>
    dplyr::count(parametro, wt = n_viol, name = "total") |>
    dplyr::arrange(dplyr::desc(total)) |>
    dplyr::slice(1) else NULL

  top_viol_point <- if (nrow(viol)) viol |>
    dplyr::count(ponto, wt = n_viol, name = "total") |>
    dplyr::arrange(dplyr::desc(total)) |>
    dplyr::slice(1) else NULL

  if (nrow(viol)) {
    p2 <- glue::glue(
      "Quanto Ã  conformidade com a ResoluÃ§Ã£o CONAMA 357/2005 (classe {classe_conama}), foram registradas violaÃ§Ãµes, com destaque para {top_viol_param$parametro[1]} como parÃ¢metro mais crÃ­tico e o ponto {top_viol_point$ponto[1]} concentrando o maior nÃºmero de ocorrÃªncias."
    )
  } else {
    p2 <- glue::glue(
      "Todos os registros avaliados atenderam aos limites estabelecidos pela ResoluÃ§Ã£o CONAMA 357/2005 (classe {classe_conama}) nos parÃ¢metros checados."
    )
  }

  p3 <- NULL
  if (incluir_tendencia && "data" %in% names(conf)) {
    tend <- calc_trends(conf, parametros = intersect(parametros_tendencia, names(conf)))
    if (nrow(tend)) {
      top <- tend |>
        dplyr::arrange(dplyr::desc(abs(beta))) |>
        dplyr::slice(1:min(2, n()))
      itens <- apply(top, 1, function(r){
        dir <- ifelse(as.numeric(r["beta"]) > 0, "tendÃªncia de aumento", "tendÃªncia de reduÃ§Ã£o")
        sig <- ifelse(as.numeric(r["p_value"]) < 0.05, " (significativa, p<0,05)", "")
        glue::glue("{r['parametro']} em {r['ponto']}: {dir}{sig}")
      })
      p3 <- glue::glue("A anÃ¡lise temporal indicou {paste(itens, collapse='; ')}.")
    }
  }

  range_iqa <- iqa_max - iqa_min
  p4 <- glue::glue("A variaÃ§Ã£o espacial do IQA foi de aproximadamente {scales::number(range_iqa, accuracy = 0.1)} pontos entre os extremos.")

  recs <- c()
  if (nrow(viol)) {
    if ("coliformes_ok" %in% ok_cols && any(conf$coliformes_ok == FALSE, na.rm = TRUE)) {
      recs <- c(recs, "reforÃ§ar controle de fontes fecais difusas e saneamento local;")
    }
    if ("od_ok" %in% ok_cols && any(conf$od_ok == FALSE, na.rm = TRUE)) {
      recs <- c(recs, "avaliar aporte orgÃ¢nico/DBO e reaeraÃ§Ã£o nos trechos afetados;")
    }
    if ("turbidez_ok" %in% ok_cols && any(conf$turbidez_ok == FALSE, na.rm = TRUE)) {
      recs <- c(recs, "inspecionar erosÃ£o de margens e manejo do solo na bacia;")
    }
  }
  p5 <- if (length(recs)) glue::glue("Recomenda-se {paste(recs, collapse = ' ')} priorizando os pontos crÃ­ticos.") else NULL

  purrr::compact(c(p1, p2, p3, p4, p5))
}

#' @keywords internal
calc_trends <- function(df, parametros){
  if (!length(parametros)) return(dplyr::tibble())
  out <- list()
  for (p in parametros) {
    if (!p %in% names(df)) next
    tmp <- df |>
      dplyr::filter(!is.na(.data[[p]]), !is.na(data)) |>
      dplyr::group_by(ponto) |>
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

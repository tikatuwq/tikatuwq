#' Parameter analysis utilities (single-parameter)
#'
#' Conjunto de funcoes para analisar **um parametro por chamada**,
#' com filtros opcionais por `rio` e `ponto`, e utilitarios de periodo.
#'
#' @keywords internal
#' @name param_analysis
NULL


# --------------------------------- Helpers -------------------------------------

#' @keywords internal
.filter_scope <- function(df, rios = NULL, pontos = NULL) {
  out <- df
  if (!is.null(rios) && "rio" %in% names(out)) {
    out <- dplyr::filter(out, .data$rio %in% rios)
  }
  if (!is.null(pontos) && "ponto" %in% names(out)) {
    out <- dplyr::filter(out, .data$ponto %in% pontos)
  }
  out
}

#' @keywords internal
.ensure_param <- function(df, parametro) {
  if (!parametro %in% names(df)) {
    stop(sprintf("Parametro '%s' nao encontrado nas colunas do data frame.", parametro), call. = FALSE)
  }
  parametro
}

#' @keywords internal
.as_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))
  suppressWarnings(as.Date(x))
}

#' @keywords internal
.period_key <- function(dates, period = c("none", "month", "quarter", "year")) {
  period <- match.arg(period)
  if (period == "none" || all(is.na(dates))) return(NULL)
  dates <- .as_date_safe(dates)
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Pacote 'lubridate' necessario para agregacao por periodo.", call. = FALSE)
  }
  if (period == "month")   return(as.character(lubridate::floor_date(dates, unit = "month")))
  if (period == "quarter") return(as.character(lubridate::floor_date(dates, unit = "quarter")))
  if (period == "year")    return(as.character(lubridate::floor_date(dates, unit = "year")))
  NULL
}

# ------------------------------- SUMMARIES -------------------------------------

#' Resumo estatistico por parametro (com filtro por rio e/ou ponto)
#'
#' @description
#' Produz resumo estatistico para **um parametro**, com opcoes de filtro por
#' `rios` e/ou `pontos` e agregacao opcional por periodo (mes/trimestre/ano),
#' quando houver coluna `data`.
#'
#' @param df Data frame com ao menos a coluna do `parametro`. Idealmente contem
#'   `ponto`, e opcionalmente `rio` e `data`.
#' @param parametro Character; nome do parametro (ex.: "turbidez", "od", "pH").
#' @param rios Vetor de nomes de rio a filtrar (opcional; usa coluna `rio` se existir).
#' @param pontos Vetor de pontos a filtrar (opcional; usa coluna `ponto` se existir).
#' @param period "none", "month", "quarter" ou "year" para agregar por periodo
#'   (requer coluna `data`).
#' @param na_rm Remover `NA` dos calculos? (default `TRUE`)
#'
#' @return Tibble com colunas de agrupamento disponiveis (rio, ponto, periodo)
#'   e metricas: `n`, `mean`, `sd`, `min`, `median`, `max`.
#' @examples
#' \dontrun{
#' data("wq_demo", package = "tikatuwq")
#' param_summary(wq_demo, "turbidez", pontos = "P1")
#' param_summary(wq_demo, "od", rios = "Rio Azul", period = "month")
#' }
#' @export
#' @family parameter-tools
param_summary <- function(df, parametro,
                          rios = NULL, pontos = NULL,
                          period = c("none", "month", "quarter", "year"),
                          na_rm = TRUE) {
  period <- match.arg(period)
  p <- .ensure_param(df, parametro)
  d <- .filter_scope(df, rios = rios, pontos = pontos)

  # cria chave de periodo se aplicavel
  if ("data" %in% names(d)) {
    key <- .period_key(d$data, period = period)
    if (!is.null(key)) d$`._period` <- key
  }

  # grupos disponiveis
  groups <- character(0)
  if ("rio"   %in% names(d)) groups <- c(groups, "rio")
  if ("ponto" %in% names(d)) groups <- c(groups, "ponto")
  if ("._period" %in% names(d)) groups <- c(groups, "._period")

  # sumarizacao
  summariser <- function(.df) {
    tibble::tibble(
      n      = sum(!is.na(.df[[p]])),
      mean   = mean(.df[[p]], na.rm = na_rm),
      sd     = stats::sd(.df[[p]], na.rm = na_rm),
      min    = suppressWarnings(min(.df[[p]], na.rm = na_rm)),
      median = stats::median(.df[[p]], na.rm = na_rm),
      max    = suppressWarnings(max(.df[[p]], na_rm = na_rm))
    )
  }

  if (length(groups)) {
    out <- d |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::group_modify(~ summariser(.x)) |>
      dplyr::ungroup()
    # renomeia coluna de periodo para algo amigavel
    if ("._period" %in% names(out)) out <- dplyr::rename(out, period = "._period")
  } else {
    out <- summariser(d)
  }

  out
}

# --------------------------------- TRENDS --------------------------------------

#' Tendencia temporal por parametro (por rio/ponto se existentes)
#'
#' @description
#' Ajusta um modelo **lm(valor ~ tempo)** para **um parametro**, retornando
#' `slope`, `p_value`, `r2` e `n`. Se existirem colunas `rio` e/ou `ponto`,
#' calcula por grupo; caso contrario, calcula geral.
#'
#' @param df Data frame com `data` e a coluna do `parametro`. Idealmente contem
#'   `ponto`, e opcionalmente `rio`.
#' @param parametro Character; nome do parametro.
#' @param rios Vetor de nomes de rio a filtrar (opcional; usa coluna `rio` se existir).
#' @param pontos Vetor de pontos a filtrar (opcional; usa coluna `ponto` se existir).
#' @param na_rm Remover `NA` antes do ajuste? (default `TRUE`)
#'
#' @return Tibble com colunas de agrupamento (quando existirem) + `slope` (por dia),
#'   `p_value`, `r2`, `n`.
#' @examples
#' \dontrun{
#' data("wq_demo", package = "tikatuwq")
#' param_trend(wq_demo, "turbidez", pontos = c("P1","P2"))
#' }
#' @export
#' @family parameter-tools
param_trend <- function(df, parametro, rios = NULL, pontos = NULL, na_rm = TRUE) {
  if (!"data" %in% names(df)) {
    stop("A coluna 'data' eh obrigatoria para calcular tendencia.", call. = FALSE)
  }
  p <- .ensure_param(df, parametro)
  d <- .filter_scope(df, rios = rios, pontos = pontos)
  d$data <- .as_date_safe(d$data)

  # grupos disponiveis
  groups <- intersect(c("rio", "ponto"), names(d))

  fit_group <- function(.df) {
    .df <- dplyr::arrange(.df, .data$data)
    if (na_rm) .df <- dplyr::filter(.df, !is.na(.data[[p]]), !is.na(.data$data))
    n <- nrow(.df)
    if (n < 4) {
      return(tibble::tibble(slope = NA_real_, p_value = NA_real_, r2 = NA_real_, n = n))
    }
    tnum <- as.numeric(.df$data) # dias
    mod  <- stats::lm(.df[[p]] ~ tnum)
    s    <- broom::glance(mod)
    co   <- broom::tidy(mod)
    slope <- co$estimate[co$term == "tnum"]
    tibble::tibble(
      slope = slope,
      p_value = s$p.value,
      r2 = s$r.squared,
      n = n
    )
  }

  if (length(groups)) {
    out <- d |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::group_modify(~ fit_group(.x)) |>
      dplyr::ungroup()
  } else {
    out <- fit_group(d)
  }

  out
}

# ---------------------------------- PLOTS --------------------------------------

#' Plot temporal de um parametro (com filtro por rio e/ou ponto)
#'
#' @description
#' Gera grafico temporal para **um parametro**, com opcoes de filtro por `rios`
#' e/ou `pontos`. Se houver mais de um ponto, a cor diferencia pontos; opcional
#' `facet = TRUE` para facetar por ponto. Pode adicionar reta de tendencia com
#' `add_trend = TRUE` (lm).
#'
#' @param df Data frame com `data` e a coluna do `parametro`. Idealmente contem
#'   `ponto`, e opcionalmente `rio`.
#' @param parametro Character; nome do parametro.
#' @param rios Vetor de nomes de rio a filtrar (opcional; usa coluna `rio` se existir).
#' @param pontos Vetor de pontos a filtrar (opcional; usa coluna `ponto` se existir).
#' @param add_trend Logical; se `TRUE`, adiciona `geom_smooth(method = "lm", se = FALSE)`.
#' @param facet Logical; se `TRUE` e houver `ponto`, aplica `facet_wrap(~ponto)`.
#'
#' @return Objeto `ggplot`.
#' @examples
#' \dontrun{
#' data("wq_demo", package = "tikatuwq")
#' param_plot(wq_demo, "turbidez", pontos = c("P1","P2"), add_trend = TRUE, facet = TRUE)
#' }
#' @export
#' @family parameter-tools
param_plot <- function(df, parametro,
                       rios = NULL, pontos = NULL,
                       add_trend = TRUE, facet = FALSE) {
  if (!"data" %in% names(df)) {
    stop("A coluna 'data' eh obrigatoria para plot temporal.", call. = FALSE)
  }
  p <- .ensure_param(df, parametro)
  d <- .filter_scope(df, rios = rios, pontos = pontos)
  d$data <- .as_date_safe(d$data)

  # esteticas
  has_ponto <- "ponto" %in% names(d)
  aes_base <- if (has_ponto) {
    ggplot2::aes(x = .data$data, y = .data[[p]], color = .data$ponto)
  } else {
    ggplot2::aes(x = .data$data, y = .data[[p]])
  }

  g <- ggplot2::ggplot(d, aes_base) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::labs(x = NULL, y = parametro)

  if (add_trend) {
    g <- g + ggplot2::geom_smooth(method = "lm", se = FALSE, na.rm = TRUE)
  }
  if (facet && has_ponto) {
    g <- g + ggplot2::facet_wrap(~ponto, scales = "free_y")
  }

  g
}

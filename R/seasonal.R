# R/seasonal.R
# Analise sazonal para qualidade da agua em bacias tropicais brasileiras
# (ASCII-only no codigo)

# Calendario hidrologico por regiao (meses do periodo chuvoso)
.season_calendar <- list(
  sudeste      = c(10L, 11L, 12L, 1L, 2L, 3L),  # Out-Mar
  centro_oeste = c(10L, 11L, 12L, 1L, 2L, 3L),  # Out-Mar
  bahia        = c(10L, 11L, 12L, 1L, 2L, 3L),  # Out-Mar (BA litoral sul/SE)
  nordeste     = c(2L,  3L,  4L,  5L, 6L, 7L),  # Fev-Jul (semiarido/intertropical)
  norte        = c(12L, 1L,  2L,  3L, 4L, 5L),  # Dez-Mai (Amazonia)
  sul          = c(6L,  7L,  8L,  9L)            # Jun-Set (inverno umido)
)

#' Atribui periodo hidrologico (chuvoso / seco) a cada amostra
#'
#' @description
#' Adiciona a coluna \code{season} ao data frame, classificando cada
#' amostra como \code{"chuvoso"} ou \code{"seco"} com base no mes de
#' coleta e no calendario hidrologico regional brasileiro.
#'
#' @details
#' Os calendarios regionais disponiveis (meses chuvosos) sao:
#' \describe{
#'   \item{\code{"sudeste"}}{Outubro-Marco (Oct-Mar).}
#'   \item{\code{"bahia"}}{Outubro-Marco; adequado para BA litoral
#'         sul, regiao do Rio Buranhem, ES.}
#'   \item{\code{"centro_oeste"}}{Outubro-Marco.}
#'   \item{\code{"nordeste"}}{Fevereiro-Julho; semiarido/intertropical.}
#'   \item{\code{"norte"}}{Dezembro-Maio; Amazonia.}
#'   \item{\code{"sul"}}{Junho-Setembro; inverno umido subtropical.}
#'   \item{\code{"custom"}}{Define os meses chuvosos pelo argumento
#'         \code{wet_months}.}
#' }
#'
#' @param df Data frame com ao menos a coluna \code{date_col}.
#' @param region Character ou \code{NULL} (default); regiao climatica usada
#'   para definir o calendario chuvoso/seco. Uma de \code{"sudeste"},
#'   \code{"bahia"}, \code{"centro_oeste"}, \code{"nordeste"},
#'   \code{"norte"}, \code{"sul"} ou \code{"custom"}. Este argumento e
#'   **opcional**: muitas analises de qualidade da agua nao consideram
#'   sazonalidade regional. Se \code{region = NULL}:
#'   \itemize{
#'     \item se \code{wet_months} for fornecido, ele e usado diretamente
#'       (equivalente a \code{region = "custom"});
#'     \item caso contrario, a coluna \code{season} e preenchida com
#'       \code{NA} e uma mensagem informativa e exibida (sem erro), de
#'       forma que o restante do fluxo de analise nao e bloqueado.
#'   }
#' @param date_col Character; nome da coluna de datas. Default \code{"data"}.
#' @param wet_months Integer vector; meses numericos do periodo chuvoso
#'   (1 = Jan ... 12 = Dez). Obrigatorio quando \code{region = "custom"}.
#'   Tambem pode ser usado com \code{region = NULL} para definir um
#'   calendario personalizado sem escolher uma regiao predefinida.
#' @param labels Character vector de comprimento 2 com os rotulos para os
#'   periodos chuvoso e seco, nesta ordem.
#'   Default \code{c("chuvoso", "seco")}.
#'
#' @returns O \code{df} de entrada com a coluna \code{season} adicionada
#'   (character, podendo ser \code{NA} quando nenhuma regiao/calendario
#'   for informado).
#'
#' @seealso \code{\link[=compare_seasons]{compare_seasons()}}
#'
#' @family seasonal-tools
#'
#' @examples
#' data("wq_demo", package = "tikatuwq")
#' d <- assign_season(wq_demo, region = "bahia")
#' table(d$season)
#'
#' # region e opcional: sem informa-la, season fica NA (sem erro)
#' d2 <- assign_season(wq_demo)
#' table(d2$season, useNA = "always")
#'
#' @export
assign_season <- function(
  df,
  region    = NULL,
  date_col  = "data",
  wet_months = NULL,
  labels    = c("chuvoso", "seco")
) {
  stopifnot(is.data.frame(df))
  if (!date_col %in% names(df)) {
    stop("Coluna '", date_col, "' nao encontrada.")
  }
  if (length(labels) != 2) stop("'labels' deve ter exatamente 2 elementos.")

  valid_regions <- c("sudeste", "bahia", "centro_oeste", "nordeste",
                      "norte", "sul", "custom")

  if (is.null(region)) {
    if (!is.null(wet_months) && length(wet_months)) {
      rainy <- as.integer(wet_months)
    } else {
      df[["season"]] <- NA_character_
      message(
        "assign_season(): nenhuma 'region' ou 'wet_months' foi informada; ",
        "a coluna 'season' foi preenchida com NA. Para classificar por ",
        "periodo hidrologico, informe region (ex.: 'bahia') ou wet_months."
      )
      return(df)
    }
  } else {
    region <- match.arg(region, valid_regions)
    if (region == "custom") {
      if (is.null(wet_months) || !length(wet_months)) {
        stop("Fornecer 'wet_months' quando region = 'custom'.")
      }
      rainy <- as.integer(wet_months)
    } else {
      rainy <- .season_calendar[[region]]
    }
  }

  dates  <- as.Date(df[[date_col]])
  months <- as.integer(format(dates, "%m"))
  df[["season"]] <- ifelse(months %in% rainy, labels[1], labels[2])
  df
}

#' Comparacao estatistica entre periodos hidrologicos
#'
#' @description
#' Compara um parametro de qualidade da agua entre os periodos chuvoso
#' e seco, com estatisticas descritivas, teste de hipotese e grafico.
#' Requer que o data frame ja tenha a coluna \code{season} (use
#' \code{assign_season()} antes).
#'
#' @details
#' O teste escolhido por \code{test} e aplicado por grupo (\code{by}).
#' Para \code{"wilcoxon"} usa \code{stats::wilcox.test()} (nao-parametrico,
#' recomendado para dados ambientais); para \code{"t_test"} usa
#' \code{stats::t.test()}; para \code{"kruskal"} usa
#' \code{stats::kruskal.test()}.
#'
#' @param df Data frame com ao menos as colunas \code{parametro},
#'   \code{season_col} e as colunas em \code{by}.
#' @param param Character; nome da coluna do parametro a comparar.
#' @param season_col Character; nome da coluna de periodo hidrologico.
#'   Default \code{"season"}.
#' @param by Character vector; colunas de agrupamento (ex.: \code{"ponto"}).
#'   Se \code{NULL}, analisa o conjunto todo sem agrupamento.
#' @param test Metodo de comparacao: \code{"wilcoxon"} (default),
#'   \code{"t_test"} ou \code{"kruskal"}.
#' @param alpha Nivel de significancia para classificar tendencia.
#'   Default \code{0.05}.
#' @param plot Logico; se \code{TRUE} (default) retorna um boxplot
#'   comparativo como atributo \code{"plot"} do resultado.
#'
#' @returns
#' Um tibble com uma linha por grupo (colunas \code{by}), contendo:
#' \describe{
#'   \item{n_total}{Total de amostras com valor valido no grupo.}
#'   \item{n_chuvoso, n_seco}{Amostras por periodo.}
#'   \item{median_chuvoso, median_seco}{Medianas por periodo.}
#'   \item{mean_chuvoso, mean_seco}{Medias por periodo.}
#'   \item{statistic}{Estatistica do teste.}
#'   \item{p_value}{P-valor do teste.}
#'   \item{diferenca_significativa}{Logical; \code{p_value < alpha}.}
#'   \item{tendencia}{Character: \code{"chuvoso_maior"}, \code{"seco_maior"}
#'     ou \code{"sem_diferenca"}.}
#' }
#' Se \code{plot = TRUE}, o atributo \code{attr(resultado, "plot")} contem
#' um objeto \code{ggplot}.
#'
#' @seealso \code{\link[=assign_season]{assign_season()}}
#'
#' @family seasonal-tools
#'
#' @importFrom stats wilcox.test t.test kruskal.test median
#' @importFrom rlang .data sym
#' @importFrom dplyr filter group_by summarise bind_rows
#' @importFrom tibble as_tibble
#'
#' @examples
#' data("wq_demo", package = "tikatuwq")
#' d <- assign_season(wq_demo, region = "bahia")
#' res <- compare_seasons(d, param = "turbidez", by = "ponto", test = "wilcoxon")
#' print(res)
#'
#' @export
compare_seasons <- function(
  df,
  param,
  season_col = "season",
  by         = "ponto",
  test       = c("wilcoxon", "t_test", "kruskal"),
  alpha      = 0.05,
  plot       = TRUE
) {
  test <- match.arg(test)
  stopifnot(is.data.frame(df))
  if (!param %in% names(df))      stop("Parametro '", param, "' nao encontrado.")
  if (!season_col %in% names(df)) stop("Coluna '", season_col, "' nao encontrada. Use assign_season() antes.")

  by_ok <- intersect(by, names(df))
  if (!length(by_ok)) {
    df[[".__grp__"]] <- "all"
    by_ok <- ".__grp__"
  }

  # Valores validos
  vals  <- suppressWarnings(as.numeric(df[[param]]))
  seas  <- df[[season_col]]

  season_levels <- sort(unique(seas[!is.na(seas)]))
  if (length(season_levels) < 2) {
    warning("Apenas um nivel de '", season_col, "' disponivel; comparacao impossivel.")
    return(tibble::tibble())
  }

  # Grupos
  grps <- split(seq_len(nrow(df)), df[by_ok], drop = TRUE)

  result_list <- lapply(grps, function(idx) {
    subv <- vals[idx]
    subs <- seas[idx]
    row_ids <- as.list(df[idx[1], by_ok, drop = FALSE])

    # Divide por periodo
    v1 <- subv[!is.na(subs) & subs == season_levels[1] & is.finite(subv)]
    v2 <- subv[!is.na(subs) & subs == season_levels[2] & is.finite(subv)]

    stat_val <- NA_real_
    p_val    <- NA_real_

    tryCatch({
      if (test == "wilcoxon") {
        if (length(v1) >= 2 && length(v2) >= 2) {
          r <- stats::wilcox.test(v1, v2, exact = FALSE)
          stat_val <- unname(r$statistic)
          p_val    <- r$p.value
        }
      } else if (test == "t_test") {
        if (length(v1) >= 2 && length(v2) >= 2) {
          r <- stats::t.test(v1, v2)
          stat_val <- unname(r$statistic)
          p_val    <- r$p.value
        }
      } else {
        all_v   <- c(v1, v2)
        all_grp <- c(rep(season_levels[1], length(v1)),
                     rep(season_levels[2], length(v2)))
        if (length(all_v) >= 4) {
          r <- stats::kruskal.test(all_v ~ as.factor(all_grp))
          stat_val <- unname(r$statistic)
          p_val    <- r$p.value
        }
      }
    }, error = function(e) invisible(NULL))

    sig       <- isTRUE(!is.na(p_val) && p_val < alpha)
    m1        <- if (length(v1)) median(v1, na.rm = TRUE) else NA_real_
    m2        <- if (length(v2)) median(v2, na.rm = TRUE) else NA_real_
    tendencia <- if (!sig) "sem_diferenca" else if (!is.na(m1) && !is.na(m2) && m1 >= m2) {
      paste0(season_levels[1], "_maior")
    } else {
      paste0(season_levels[2], "_maior")
    }

    base <- as.data.frame(row_ids, stringsAsFactors = FALSE)
    base[[paste0("n_", season_levels[1])]]      <- length(v1)
    base[[paste0("n_", season_levels[2])]]      <- length(v2)
    base[["n_total"]]                           <- length(v1) + length(v2)
    base[[paste0("median_", season_levels[1])]] <- round(m1, 3)
    base[[paste0("median_", season_levels[2])]] <- round(m2, 3)
    base[[paste0("mean_",   season_levels[1])]] <- round(mean(v1, na.rm = TRUE), 3)
    base[[paste0("mean_",   season_levels[2])]] <- round(mean(v2, na.rm = TRUE), 3)
    base[["statistic"]]                         <- round(stat_val, 4)
    base[["p_value"]]                           <- round(p_val, 4)
    base[["diferenca_significativa"]]           <- sig
    base[["tendencia"]]                         <- tendencia
    base
  })

  out <- tibble::as_tibble(dplyr::bind_rows(result_list))
  out[[".__grp__"]] <- NULL

  # Grafico comparativo
  if (isTRUE(plot)) {
    plot_df      <- df
    plot_df[[".val."]] <- vals
    seas_sym     <- rlang::sym(season_col)
    p_sym        <- rlang::sym(".val.")

    gg_base <- ggplot2::ggplot(
      plot_df[!is.na(vals) & !is.na(seas), , drop = FALSE],
      ggplot2::aes(x = !!seas_sym, y = !!p_sym, fill = !!seas_sym)
    ) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = 21) +
      ggplot2::geom_jitter(width = 0.15, size = 1.5, alpha = 0.5, shape = 16) +
      ggplot2::labs(
        x     = "Periodo hidrologico",
        y     = param,
        fill  = "Periodo",
        title = paste0("Comparacao sazonal: ", param),
        subtitle = paste0("Teste: ", test,
                          if (!is.na(out$p_value[1])) paste0(" | p = ", round(out$p_value[1], 3)) else "")
      ) +
      ggplot2::scale_fill_manual(
        values = c("chuvoso" = "#4C9BE8", "seco" = "#E89A4C"),
        na.value = "grey70"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")

    if (length(by_ok) && !(".__grp__" %in% by_ok)) {
      gg_base <- gg_base + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(by_ok[1])))
    }

    attr(out, "plot") <- gg_base
  }

  out
}

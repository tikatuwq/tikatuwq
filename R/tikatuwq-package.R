#' tikatuwq: Ferramentas para analise de qualidade da agua (Brasil)
#'
#' Indices (IQA CETESB/NSF, IET Carlson/Lamparelli), checagem CONAMA 357/2005,
#' visualizacoes e texto analitico automatico (rule-based).
#'
#' @keywords internal
#'
#' @importFrom stats approx median sd
#' @importFrom utils data
#' @importFrom dplyr coalesce n all_of
#' @importFrom readr read_lines read_delim parse_number cols col_character locale
#' @importFrom stringr str_count str_replace_all
#' @importFrom lubridate ymd dmy
#' @importFrom tibble as_tibble
#' @importFrom rlang warn
#' @importFrom scales label_number label_percent
"_PACKAGE"

# Silencia NOTEs de variaveis NSE usadas com dplyr/ggplot2
utils::globalVariables(c(
  ".data","IQA","IQA_med","param_ok","ok","parametro","n_viol","total",
  "ponto","data","valor","status","viol","delta","idx","lim_min","lim_max"
))

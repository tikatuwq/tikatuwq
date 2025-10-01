# R/tikatuwq-package.R
# ASCII-only in code/comments.

#' tikatuwq: Water quality tools for the Brazilian context
#'
#' @description
#' Utilities to import, clean, validate and analyze freshwater quality data.
#' Includes indices (IQA/WQI, TSI/IET Carlson and Lamparelli), compliance
#' checks against CONAMA 357/2005, visualizations, and rule-based analytical text.
#'
#' @section Main features:
#' - **Indices**: IQA/WQI; TSI/IET (Carlson, Lamparelli); NSF WQI prototype.  
#' - **Compliance**: CONAMA 357/2005 limits and per-record status.  
#' - **Visualization**: time series, boxplots, heatmap, IQA bars.  
#' - **Reporting**: simple Rmd/Quarto report; analytical paragraphs (rule-based).  
#'
#' @section Quick start:
#' \preformatted{
#' # demo data
#' data(wq_demo)
#'
#' # compute IQA
#' d1 <- iqa(wq_demo, na_rm = TRUE)
#'
#' # check compliance (CONAMA class "2")
#' d2 <- conama_check(d1, classe = "2")
#'
#' # summary table (only violations)
#' conama_report(d2, classe = "2", only_violations = TRUE)
#' }
#'
#' @section Vignettes:
#' See the package website for walkthroughs and examples:
#' \href{https://tikatuwq.github.io/tikatuwq/}{tikatuwq website}.
#'
#' @seealso
#' [read_wq()], [conama_check()], [iqa()],
#' [iet_carlson()], [iet_lamparelli()],
#' [plot_series()], [render_report()]
#'
#' @references
#' Carlson (1977) <doi:10.4319/lo.1977.22.2.0361>  
#' Lamparelli (2004) <https://www.teses.usp.br/teses/disponiveis/41/41134/tde-20032006-075813/publico/TeseLamparelli2004.pdf>  
#' NSF WQI <https://link.springer.com/article/10.1007/s11157-023-09650-7>  
#' CONAMA 357/2005 <https://conama.mma.gov.br/?id=450&option=com_sisconama&task=arquivo.download>
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
#'
"_PACKAGE"

# Silence NSE notes for dplyr/ggplot2 columns/symbols
utils::globalVariables(c(
  ".data",
  # analysis_text helpers
  "IQA", "IQA_med", "param_ok", "ok", "parametro", "n_viol", "total",
  "ponto", "data", "valor", "status", "viol", "delta", "idx", "lim_min", "lim_max",
  # viz and summaries
  "mes"
))

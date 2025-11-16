#' Example water quality dataset (subset of real data)
#'
#' A small subset of real monitoring data used in examples and vignettes. Now includes extra columns `rio`, `lat`, `lon`.
#'
#' @format A tibble/data.frame with 20 rows and 14 columns:
#' \describe{
#'   \item{rio}{character, river name}
#'   \item{ponto}{character, monitoring point id}
#'   \item{data}{Date, sampling date}
#'   \item{ph}{numeric, pH}
#'   \item{od}{numeric, dissolved oxygen (mg/L)}
#'   \item{turbidez}{numeric, NTU}
#'   \item{dbo}{numeric, mg/L}
#'   \item{coliformes}{numeric, MPN/100 mL}
#'   \item{p_total}{numeric, total phosphorus (mg/L)}
#'   \item{nt_total}{numeric, total nitrogen (mg/L)}
#'   \item{temperatura}{numeric, degrees Celsius}
#'   \item{tds}{numeric, total dissolved solids (mg/L)}
#'   \item{lat}{numeric, latitude}
#'   \item{lon}{numeric, longitude}
#' }
#'
#' @usage data(wq_demo)
#'
#' @details
#' The dataset is a real subset selected from BURANHEM river (dataset-real.csv), used for reproducible examples and vignettes.
#' Covers 4 monitoring points and years 2020–2024. All core columns for IQA/CONAMA/plotting helpers are present.
#'
#' @source Subset of dataset-real.csv (BURANHEM river, 4 sites, years 2020–2024).
#'
#' @seealso \code{\link[=iqa]{iqa()}}, \code{\link[=conama_check]{conama_check()}}, \code{\link[=plot_series]{plot_series()}},
#'   \code{\link[=plot_box]{plot_box()}}, \code{\link[=plot_iqa]{plot_iqa()}}, \code{\link[=plot_heatmap]{plot_heatmap()}}
#'
#' @examples
#' data("wq_demo", package = "tikatuwq")
#' head(wq_demo)
#' # quick IQA example:
#' # iqa(wq_demo, na_rm = TRUE)
#'
#' @docType data
#' @keywords datasets
"wq_demo"

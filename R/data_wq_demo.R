#' Demo water quality dataset
#'
#' A tiny example dataset used in examples and the vignette. Columns are
#' aligned with the package functions (Portuguese variable names).
#'
#' @format A data frame with 20 rows and 11 variables:
#' \describe{
#'   \item{ponto}{chr, monitoring point id}
#'   \item{data}{Date, sampling date}
#'   \item{ph}{numeric}
#'   \item{od}{numeric, dissolved oxygen (mg/L)}
#'   \item{turbidez}{numeric, NTU}
#'   \item{dbo}{numeric, mg/L}
#'   \item{coliformes}{integer, MPN/100 mL}
#'   \item{p_total}{numeric, mg/L}
#'   \item{nt_total}{numeric, mg/L}
#'   \item{temperatura}{numeric, Celsius}
#'   \item{tds}{numeric, total dissolved solids (mg/L)}
#' }
#' @usage data(wq_demo)
#' @docType data
#' @keywords datasets
#' @source Simulated for package examples.
"wq_demo"

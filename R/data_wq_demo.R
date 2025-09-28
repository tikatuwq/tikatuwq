#' Demo water quality dataset
#'
#' A tiny example dataset used in examples and vignettes. Column names follow
#' the package's Portuguese conventions (e.g., `ponto`, `data`, `turbidez`).
#'
#' @format A data frame (tibble) with 20 rows and 11 columns:
#' \describe{
#'   \item{ponto}{character, monitoring point id}
#'   \item{data}{Date, sampling date}
#'   \item{ph}{numeric, pH}
#'   \item{od}{numeric, dissolved oxygen (mg/L)}
#'   \item{turbidez}{numeric, NTU}
#'   \item{dbo}{numeric, mg/L}
#'   \item{coliformes}{integer, MPN/100 mL}
#'   \item{p_total}{numeric, total phosphorus (mg/L)}
#'   \item{nt_total}{numeric, total nitrogen (mg/L)}
#'   \item{temperatura}{numeric, degrees Celsius}
#'   \item{tds}{numeric, total dissolved solids (mg/L)}
#' }
#'
#' @usage data(wq_demo)
#'
#' @details
#' The dataset is simulated for illustrative purposes and is suitable for
#' quick examples of \code{iqa()}, \code{conama_check()}, and plotting helpers.
#'
#' @source Simulated for package examples.
#'
#' @seealso [=iqa]{iqa()}, [=conama_check]{conama_check()}, [=plot_series]{plot_series()},
#'   [=plot_box]{plot_box()}, [=plot_iqa]{plot_iqa()}, [=plot_heatmap]{plot_heatmap()}
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

#' Render a report (R Markdown / Quarto) from the internal template
#'
#' By default, writes to a temporary directory to comply with CRAN policies.
#' The function returns the full path to the generated file.
#' @param df Data frame with input data used by the template
#' @param meta Named list with contextual metadata (e.g., river, period)
#' @param output_file Output file name (default 'wq_report.html')
#' @param output_dir Directory where the file will be written (default tempdir())
#' @param template Path to the internal template file
#' @return Invisible character path to the generated report
#' @export
render_report <- function(df,
                          meta = list(river = NA, period = NA),
                          output_file = "wq_report.html",
                          output_dir = tempdir(),
                          template = system.file("templates","report_rmd.Rmd", package = "tikatuwq")){
  if (!file.exists(template)) stop("Template not found: ", template)
  rmarkdown::render(input = template,
                    output_file = output_file,
                    output_dir = output_dir,
                    params = list(data = df, meta = meta),
                    quiet = TRUE)
  invisible(file.path(output_dir, output_file))
}



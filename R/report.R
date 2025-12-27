# R/report.R
# ASCII-only in code/comments.

#' Render a water-quality report from the internal R Markdown template
#'
#' @description
#' Renders an HTML report using the package's internal R Markdown template.
#' By default, the output is written to a **temporary directory** to comply
#' with CRAN policies. The function returns (invisibly) the full path to the
#' generated file.
#'
#' @details
#' The template expects a data frame with columns compatible with the package
#' (e.g., `ponto`, `data`, parameters used by IQA/CONAMA checks). You can pass
#' optional metadata via `meta`, such as `river` and `period`.
#'
#' This function relies on **rmarkdown** (listed in Suggests). If the package
#' is not available, an informative error is thrown.
#'
#' @param df Data frame with the input data used by the template.
#' @param meta Named list with contextual metadata (e.g., `river`, `period`).
#' @param output_file File name for the report (default `"wq_report.html"`).
#' @param output_dir Directory where the file will be written (default `tempdir()`).
#'   It will be created if it does not exist.
#' @param template Path to the internal template file. Defaults to the package
#'   Rmd template shipped under `inst/templates/report_rmd.Rmd`.
#'
#' @returns Invisible character string: the absolute path to the generated report.
#'
#' @section Notes:
#' - The default output directory is `tempdir()` to comply with CRAN policies.
#'   All files (including intermediate files generated during rendering) are
#'   written only to `output_dir` or temporary directories, never to the package
#'   installation directory.
#' - The template is an **Rmd** (R Markdown). If you prefer Quarto, provide a
#'   custom `template` path to a `.qmd` and ensure your environment supports it.
#'
#' @examplesIf requireNamespace("rmarkdown", quietly = TRUE)
#' # Minimal example (writes to a temporary directory)
#' d <- wq_demo
#' path <- render_report(d, meta = list(river = "Example River", period = "Jan–Feb"))
#' file.exists(path)
#'
#' @seealso \code{rmarkdown::render()}
#' @family reporting
#' @export
render_report <- function(
  df,
  meta = list(river = NA, period = NA),
  output_file = "wq_report.html",
  output_dir = tempdir(),
  template = system.file("templates", "report_rmd.Rmd", package = "tikatuwq")
) {
  # sanity checks
  if (!nzchar(template) || !file.exists(template)) {
    stop("Template not found: ", template)
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required to render the report. ",
         "Please install it: install.packages('rmarkdown')", call. = FALSE)
  }
  if (!dir.exists(output_dir)) {
    ok <- dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (!ok) stop("Could not create output_dir: ", output_dir)
  }

  # CRAN compliance: copy template to writable temp directory
  # This prevents rmarkdown::render() from writing intermediate files
  # to the read-only package installation directory
  template_dir <- tempfile("tikatuwq_template_")
  dir.create(template_dir, showWarnings = FALSE)
  on.exit(unlink(template_dir, recursive = TRUE), add = TRUE)
  
  template_basename <- basename(template)
  template_copy <- file.path(template_dir, template_basename)
  file.copy(template, template_copy, overwrite = TRUE)
  
  # Ensure output_file has no path components (only basename)
  output_file <- basename(output_file)
  if (!nzchar(output_file)) {
    output_file <- "wq_report.html"
  }

  # run render with isolated environment and explicit output settings
  out <- rmarkdown::render(
    input       = template_copy,
    output_file = output_file,
    output_dir  = output_dir,
    params      = list(data = df, meta = meta),
    quiet       = TRUE,
    envir       = new.env(parent = globalenv())
  )

  # rmarkdown::render returns the full path (character)
  invisible(normalizePath(out, winslash = "/", mustWork = FALSE))
}

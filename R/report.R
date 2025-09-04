#' Gera relatÃƒÂ³rio (R Markdown / Quarto) a partir de template
#' @export
render_report <- function(df, meta = list(rio="BuranhÃƒÂ©m", periodo="2025-01"),
                          output = "wq_report.html",
                          template = system.file("templates","report_qmd.qmd", package="tikatuwq")){
  if (!file.exists(template)) stop("Template nÃƒÂ£o encontrado: ", template)
  rmarkdown::render(input = template,
                    output_file = output,
                    params = list(data = df, meta = meta),
                    quiet = TRUE)
  invisible(output)
}

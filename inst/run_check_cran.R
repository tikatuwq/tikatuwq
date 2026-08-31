# =============================================================================
# Script de verificacao e submissao ao CRAN — tikatuwq v0.9.0
# Execute este script no RStudio com setwd() apontando para C:/BIBLIOTECA/tikatuwq
# =============================================================================

pkg_path <- "C:/BIBLIOTECA/tikatuwq"

# 1. Instalar dependencias necessarias (se ainda nao tiver)
required <- c("devtools", "testthat", "rcmdcheck", "urlchecker",
              "dplyr", "ggplot2", "tidyr", "readr", "lubridate",
              "stringr", "glue", "scales", "broom", "purrr", "tibble",
              "rlang", "leaflet", "withr", "rmarkdown")
to_install <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

# 2. Carregar e testar
message("=== devtools::load_all() ===")
devtools::load_all(pkg_path)

message("\n=== devtools::test() ===")
test_results <- devtools::test(pkg_path)
print(test_results)

# 3. R CMD check --as-cran
message("\n=== devtools::check() --as-cran ===")
check_results <- devtools::check(
  pkg_path,
  args      = "--as-cran",
  error_on  = "note"   # falha em qualquer NOTE, WARNING ou ERROR
)

# 4. Verificar URLs
message("\n=== urlchecker::url_check() ===")
if (requireNamespace("urlchecker", quietly = TRUE)) {
  urlchecker::url_check(pkg_path)
}

# 5. Checar no win-builder (R-devel, CRAN usa este ambiente)
message("\n=== devtools::check_win_devel() ===")
devtools::check_win_devel(pkg_path)  # envia email com resultado em ~30 min

message("\n=== TUDO OK? Pronto para submeter! ===")
message("Execute: devtools::submit_cran(pkg_path)")

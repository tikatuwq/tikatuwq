# Tikatu Water Quality (tikatuwq)

R package for water quality analysis in the Brazilian context: WQI (IQA), TSI (Carlson/Lamparelli), NSFWQI, CONAMA 357/2005 limits, visualizations, reports, and automatic rule-based text generation.

📄 [Leia em Português](https://github.com/tikatuwq/tikatuwq/blob/main/README-pt.md)

<!-- CRAN status -->
[![CRAN status](https://www.r-pkg.org/badges/version/tikatuwq)](https://cran.r-project.org/package=tikatuwq)

<!-- CRAN logs - downloads -->
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/tikatuwq)](https://cran.r-project.org/package=tikatuwq)
[![CRAN monthly downloads](https://cranlogs.r-pkg.org/badges/tikatuwq)](https://cran.r-project.org/package=tikatuwq)

<!-- License -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- R CMD check results -->
[![R-CMD-check](https://github.com/tikatuwq/tikatuwq/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tikatuwq/tikatuwq/actions)

<!-- Lifecycle -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)


## Development Installation
```r
install.packages(c("devtools","testthat","rmarkdown","ggplot2","dplyr","tidyr",
                   "readr","lubridate","stringr","glue","scales","broom","purrr"))
devtools::load_all("tikatuwq")
devtools::check("tikatuwq")
```

## Basic Workflow
```r
df <- read_wq(system.file("extdata","exemplo_chamagunga.csv", package="tikatuwq")) |>
  validate_wq() |>
  iqa(na_rm = TRUE)

plot_iqa(df)

# Rule-based analytical text (no AI)
pars <- generate_analysis(df, classe_conama = "2",
                          incluir_tendencia = TRUE,
                          parametros_tendencia = c("turbidez","od","pH"),
                          contexto = list(river="Chamagunga", period="2025-07"))
cat(paste(pars, collapse = "\n\n"))
```

---

## News


### News v0.6.2 (current)

- Patch release requested by CRAN.
- Fixed a codoc WARNING in `generate_analysis()` documentation.
- Removed outdated parameters `id_cols` and `filter` to match the current function signature.
- No functional or code changes were made.

### News v0.6.1

- Maintenance update requested by CRAN.
- Fixed a NOTE by replacing the relative link `README-pt.md` with an absolute HTTPS URL.
- No functional or code changes were made.

### What's new in v0.6.0 (previous)
- New function `plot_trend()` for temporal trend analysis:
  - Trend lines by parameter/site using **Theil-Sen**, **OLS**, and **LOESS** methods.
  - Support for faceting by river/site and point customization.
  - Returns a `ggplot` object, ready for visualization or inclusion in reports.
- Updated documentation and examples on the pkgdown site.

### v0.5.1
- Fixed **invalid links** reported by CRAN (updated URLs and added DOIs).
- Minor adjustments in `tikatuwq-package.Rd` for R-devel compatibility.

### v0.5.0
- Added internal **helpers** for simplified workflow.
- New function `plot_map()` for spatial visualization of sampling points.
- Reviewed and standardized validation messages.

### v0.2.1
- `generate_analysis()` — automatic rule-based paragraphs.
- Updated report template including textual analysis.
- Added structures for `iet_lamparelli()` and `nsfwqi()`.

---

## GitHub Installation

```r
install.packages("remotes")  # or devtools
remotes::install_github("tikatuwq/tikatuwq", dependencies = TRUE)

# Stable release (by tag)
remotes::install_github("tikatuwq/tikatuwq@v0.5.1", build_vignettes = TRUE)
```

---

### CONAMA Compliance (class 2)
```r
df <- read_wq(system.file("extdata","exemplo_chamagunga.csv", package="tikatuwq"))

# Table with only violations, ready for reports
conama_report(df, "2", only_violations = TRUE, pretty = TRUE)

# Short textual summary
cat(paste(conama_text(df, "2", only_violations = TRUE), collapse = "\n"))
```

---

## Main Functions

- `read_wq(path)` — read water quality data (CSV).
- `validate_wq(df)` — validate/normalize columns and units.
- `iqa(df, na_rm = TRUE, ...)` — Water Quality Index (CETESB/NSF).
- `iet_carlson(df)` / `iet_lamparelli(df)` — Trophic State Index.
- `nsfwqi(df)` — NSFWQI (structure ready).
- `conama_limits(classe)` — limits from CONAMA Resolution 357/2005.
- `conama_check(df, classe)` — compliance by parameter (*_ok).
- Visualizations: `plot_iqa()`, `plot_series()`, `plot_box()`, `plot_heatmap()`, `plot_map()`.
- Reports/text: `generate_analysis()`, `render_report()`.
- Example data: `system.file("extdata", "exemplo_chamagunga.csv", package = "tikatuwq")`.

---

## Documentation and Support

- Website (pkgdown): https://tikatuwq.github.io/tikatuwq/
- Issues/suggestions: https://github.com/tikatuwq/tikatuwq/issues
- Releases: https://github.com/tikatuwq/tikatuwq/releases

## How to Cite
```r
citation("tikatuwq")
```

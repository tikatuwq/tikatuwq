# 💧 TikatuWQ: An R Package for Water Quality Assessment and Environmental Compliance in Brazil

**TikatuWQ** is an open-source R package designed to analyze, visualize, and report water quality data according to Brazilian environmental standards.  
It implements the main indices used in the country **IQA/NSFWQI** and **IET (Carlson and Lamparelli)** and provides automated checks for **CONAMA Resolution 357/2005** compliance.  
The package also includes trend analysis, data validation, and automatic report generation.

📄 [Ler em Português](https://github.com/tikatuwq/tikatuwq/blob/main/README-pt.md)

<!-- Zenodo DOI -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17407297.svg)](https://doi.org/10.5281/zenodo.17407297)

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

---

## Installation (development)

To install development dependencies and check the package locally:

```r
install.packages(c("devtools","testthat","rmarkdown","ggplot2","dplyr","tidyr",
                   "readr","lubridate","stringr","glue","scales","broom","purrr"))
devtools::load_all("tikatuwq")
devtools::check("tikatuwq")
```

---

## Basic workflow

```r
df <- read_wq(system.file("extdata","exemplo_chamagunga.csv", package="tikatuwq")) |>
  validate_wq() |>
  iqa(na_rm = TRUE)

plot_iqa(df)

# Analytical text (rule-based)
pars <- generate_analysis(df, classe_conama = "2",
                          incluir_tendencia = TRUE,
                          parametros_tendencia = c("turbidez","od","pH"),
                          contexto = list(river="Chamagunga", period="2025-07"))
cat(paste(pars, collapse = "\n\n"))
```

---

### 📚 Scientific and Institutional Context

The **TikatuWQ** package was developed by **Vinícius Saraiva Santos** (author and maintainer)  
as part of the **Tikatu Research Project**, conducted within the **Nucleus for Research in Tropical Ecosystems – NuPEcoTropic**, a research group linked to the **Federal University of Southern Bahia (UFSB)** and coordinated by **Prof. Dr. Fabrício Berton Zanchi**.  

This work was carried out as part of the activities of the **Postgraduate Program in Biosystems (PPG Biossistemas)** at UFSB, under the supervision of **Prof. Dr. Fabrício Berton Zanchi**.  

The **Tikatu Project**, developed and coordinated by **Vinícius Saraiva Santos**, integrates research focused on environmental monitoring and modeling.

---

## 🆕 News

## 🆕 What's new in v0.7.2

- Fixed CRAN incoming NOTE:
  - Removed non-standard fields (`DOI`, `Citation`) from `DESCRIPTION`.
  - Updated `inst/CITATION` to use `bibentry()` (replacing deprecated `citEntry()`).
- No functional or API changes.
- Minor documentation improvements.
- R CMD check: **0 errors | 0 warnings | 0 notes** ✅

### News v0.7.0
- New functions **`param_analysis()`** and **`param_analysis_multi()`**:
  - Allow flexible parameter-based analyses by site or river.
  - Support cross-comparisons (multiple parameters per site or vice versa).
  - Include descriptive statistics and temporal trend detection.
- Full test coverage for new modules.
- Minor improvements to `plot_trend()` and `plot_map()` (controlled messages).
- All CRAN and `devtools::check()` validations passed without errors.

### News v0.6.2
- Corrective update requested by CRAN.
- Fixed **codoc** WARNING in `generate_analysis()` documentation.
- Removed deprecated parameters `id_cols` and `filter` to match current signature.
- No functional or code changes made.

### News v0.6.1 
- Maintenance update requested by CRAN.
- Fixed relative link `README-pt.md`, now converted to absolute HTTPS URL.
- No functional or code changes made.

### News v0.6.0
- New function `plot_trend()` for temporal trend analysis:
  - Trend lines per parameter/site with **Theil-Sen**, **OLS**, and **LOESS** methods.
  - Supports faceting by river/site and point customization.
  - Returns `ggplot` object ready for visualization or reports.
- Updated documentation and examples on the pkgdown site.

### v0.5.1
- Fixed **invalid URLs** reported by CRAN (updated links and DOIs included).
- Minor documentation adjustments for R-devel compatibility.

### v0.5.0
- Added internal **helper** functions to simplify workflow.
- New feature `plot_map()` for spatial visualization of sampling sites.
- Revised validation messages and standardized formatting.

### v0.2.1
- `generate_analysis()` — automatic rule-based analytical paragraphs.
- Report template updated to include textual analysis.
- Structures added for `iet_lamparelli()` and `nsfwqi()`.

---

## Installation via GitHub

```r
install.packages("remotes")  # or devtools
remotes::install_github("tikatuwq/tikatuwq", dependencies = TRUE)

# stable version (by tag)
remotes::install_github("tikatuwq/tikatuwq@v0.7.0", build_vignettes = TRUE)
```

---

### CONAMA compliance (class 2)

```r
df <- read_wq(system.file("extdata","exemplo_chamagunga.csv", package="tikatuwq"))

# Table with only violations, ready for report
conama_report(df, "2", only_violations = TRUE, pretty = TRUE)

# Short textual summary
cat(paste(conama_text(df, "2", only_violations = TRUE), collapse = "\n"))
```

---

## Main functions

- `read_wq(path)` — reads water quality datasets (CSV).  
- `validate_wq(df)` — validates/normalizes columns and units.  
- `iqa(df, na_rm = TRUE, ...)` — Water Quality Index (CETESB/NSF).  
- `iet_carlson(df)` / `iet_lamparelli(df)` — Trophic State Index.  
- `nsfwqi(df)` — NSFWQI (framework ready).  
- `conama_limits(class)` — limits from CONAMA 357/2005 Resolution.  
- `conama_check(df, class)` — compliance by parameter (*_ok columns).  
- Visualizations: `plot_iqa()`, `plot_series()`, `plot_box()`, `plot_heatmap()`, `plot_map()`, `plot_trend()` (returns `ggplot` object).  
- Reporting/Text: `generate_analysis()`, `render_report()`.  
- Example data: `system.file("extdata", "exemplo_chamagunga.csv", package = "tikatuwq")`.

---

## Documentation and support

- **CRAN page:** https://cran.r-project.org/package=tikatuwq  
- **Pkgdown site:** https://tikatuwq.github.io/tikatuwq/  
- **Issues/Suggestions:** https://github.com/tikatuwq/tikatuwq/issues  
- **Releases:** https://github.com/tikatuwq/tikatuwq/releases  

---

## Citation
```r
citation("tikatuwq")
```

### How to cite

If you use **tikatuwq** in your research, please cite it as follows:

> Santos, V. S. (2025). *tikatuwq: Water Quality Assessment and Environmental Compliance in Brazil* (v0.7.0). Zenodo. [https://doi.org/10.5281/zenodo.17407297](https://doi.org/10.5281/zenodo.17407297)

BibTeX entry:

```bibtex
@Manual{Santos2025tikatuwq,
  title  = {tikatuwq: Water Quality Assessment and Environmental Compliance in Brazil},
  author = {Vinicius Saraiva Santos},
  year   = {2025},
  note   = {R package version 0.7.0},
  doi    = {10.5281/zenodo.17407297},
  url    = {https://github.com/tikatuwq/tikatuwq},
}
```

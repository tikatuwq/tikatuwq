# 💧 TikatuWQ: An R Package for Water Quality Assessment and Environmental Compliance in Brazil

**TikatuWQ** is an open-source R package designed to analyze, visualize, and report water quality data according to Brazilian environmental standards.  
It implements the main indices used in the country **IQA/NSFWQI** and **IET (Carlson and Lamparelli)** and provides automated checks for **CONAMA Resolution 357/2005** compliance — including the legal frequency rule (Art. 15).  
The package also includes seasonal analysis, pollutant load computation, exceedance probability, multivariate PCA, trend analysis, data validation, and automatic report generation.

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
                   "readr","lubridate","stringr","glue","scales","broom","purrr","tools"))
devtools::load_all("tikatuwq")
devtools::check("tikatuwq")
```

---

## Included real dataset: Rio Buranhem - INEMA

This package includes a real water quality dataset extracted from INEMA (the Bahia State Environmental Agency) monitoring campaigns conducted in the Rio Buranhem watershed (Porto Seguro, Bahia, Brazil) between 2021 and 2024. The included data provide site IDs, sampling dates, and diverse physicochemical variables measured during field campaigns. See documentation for `wq_demo` for details on columns and usage in analyses.

All main documentation and vignettes use this realistic sample for demonstration and reproducible workflows.

## Basic workflow

```r
library(tikatuwq)
data(wq_demo)
head(wq_demo)

# Typical analysis
wq_demo |> validate_wq() |> iqa(na_rm = TRUE) |> plot_iqa()

# Trophic state visualization
wq_demo |> iet_carlson(.keep_ids = TRUE) |> plot_iet(method = "carlson")

# Seasonal analysis
wq_demo |>
  assign_season(region = "bahia") |>
  compare_seasons(param = "turbidez", by = "ponto")
```

---

### 📚 Scientific and Institutional Context

The **TikatuWQ** package was developed by **Vinícius Saraiva Santos** (author and maintainer)  
as part of the **Tikatu Research Project**, conducted within the **Nucleus for Research in Tropical Ecosystems – NuPEcoTropic**, a research group linked to the **Federal University of Southern Bahia (UFSB)** and coordinated by **Prof. Dr. Fabrício Berton Zanchi**.  

This work was carried out as part of the activities of the **Postgraduate Program in Biosystems (PPG Biossistemas)** at UFSB, under the supervision of **Prof. Dr. Fabrício Berton Zanchi**.  

The **Tikatu Project**, developed and coordinated by **Vinícius Saraiva Santos**, integrates research focused on environmental monitoring and modeling.

---

## 🆕 News

### 🆕 What's new in v0.9.0 (current)

**⚠️ Breaking change — IQA now uses the correct weighted geometric mean**

`iqa()` now defaults to `method = "CETESB"`, which computes the weighted geometric mean `∏(Qi^Wi)` as specified by CETESB and the original NSF WQI formulation (Brown et al., 1970). The previous (incorrect) arithmetic mean behavior is preserved via `method = "NSF_approx"`. Users relying on the default will see more accurate — and generally lower — IQA values.

**New functions:**

- `conama_freq_check()` — implements the legal frequency rule of CONAMA 357/2005 Art. 15: a parameter is considered in conformity only when ≥ 80% of at least 6 samples per year are within limits. Returns a per-point, per-year, per-parameter compliance table.
- `assign_season()` — classifies each sample as `"chuvoso"` (wet) or `"seco"` (dry) based on regional Brazilian hydrological calendars (Sudeste, Nordeste, Norte, Sul, Centro-Oeste, Bahia) or a custom month vector.
- `compare_seasons()` — compares a water quality parameter between wet and dry seasons using Wilcoxon, t-test, or Kruskal-Wallis; returns descriptive statistics, test results, and an optional `ggplot` boxplot.
- `plot_iet()` — bar chart (vertical or horizontal) for the Trophic State Index with color-coded trophic classes, supporting both Carlson (1977) and Lamparelli (2004) classification schemes.
- `compute_load()` — computes pollutant load as concentration × flow × unit factor; supports kg/day, t/day, kg/year, and g/s outputs.
- `exceedance_prob()` — estimates empirical exceedance probability with Wilson confidence interval, by group.
- `wq_pca()` — PCA wrapper around `stats::prcomp()` with automatic column selection, biplot, screeplot, and loadings plot returned as `ggplot` attributes.
- `nsfwqi()` — updated: now uses weighted geometric mean (consistent with the IQA fix); adds `add_status` and `locale` arguments for multilingual status labels.

**Data expanded:**

- `inst/extdata/conama_limits.csv` expanded from ~38 to ~116 rows, adding nitrogen species (NO₃, NO₂, NH₃ with pH-conditional limits), inorganic ions (fluorides, chlorides, sulfates), organic pollutants (phenols, surfactants), and 14 heavy metals for Classes 1–3.

✔️ `R CMD check --as-cran`: **0 errors | 0 warnings | 0 notes**  
✔️ Compatible with CRAN, Windows, Linux, and macOS

---

### 🆕 What's new in v0.8.2

- CRAN maintenance: Fixed example in `plot_map()` to use internal dataset `wq_demo` instead of external file reference. All examples and tests now comply with CRAN policies.

### 🆕 What's new in v0.8.1

- Internal adjustments to ensure full compliance with CRAN policies regarding file system access.
- Examples and documentation now rely exclusively on the internal dataset `wq_demo`, removing any dependency on external or local files.
- The `render_report()` function now writes output **only to temporary directories** (`tempdir()`) or to directories explicitly provided by the user.
- Examples, tests, and documentation were reviewed to guarantee safe execution in restricted environments (e.g., CRAN check systems).
- No API changes and no functional impact for users.

✔️ `R CMD check --as-cran`: **0 errors | 0 warnings | 0 notes**  
✔️ Compatible with CRAN, Windows, Linux, and macOS


### 🆕 What's new in v0.8.0

- The `wq_demo` example dataset is now a subset of real data (INEMA, Rio Buranhem, Porto Seguro-BA, 2021–2024), with 20 rows and 14 columns (including `rio`, `lat`, `lon`).
- All examples and vignettes use this realistic dataset to improve reproducibility and clarity.
- Documentation updated accordingly (dataset help, README, vignette).
- No API break; behavior remains consistent with previous versions.

### What's new in v0.7.3

- More robust IQA
Accepts `temp` as an alias for `temperatura`.
Automatic numeric sanitization for comma decimals and `<`/`>` signs.
With `na_rm = TRUE`, weights are re-scaled if some inputs are missing.
- IET (Carlson / Lamparelli) with data.frame input
`iet_carlson()` and `iet_lamparelli()` now accept a "raw" data.frame containing extra ID columns like `rio`, `ponto`, `data`, `lat`, `lon`.
- Relevant parameters are auto-detected (`secchi/sd`, `clorofila/chla`, `tp/p_total`).
`p_total` in mg/L is auto-converted to `tp` in µg/L.
- Use `.keep_ids = TRUE` to preserve identifiers in the output.
No new dependencies, no API break.
Legacy vector calls keep working as before.

### News v0.7.2

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
remotes::install_github("tikatuwq/tikatuwq@v0.9.0", build_vignettes = TRUE)
```

---

### CONAMA compliance (class 2)

```r
df <- read_wq(system.file("extdata","exemplo_chamagunga.csv", package="tikatuwq"))

# Table with only violations, ready for report
conama_report(df, "2", only_violations = TRUE, pretty = TRUE)

# Short textual summary
cat(paste(conama_text(df, "2", only_violations = TRUE), collapse = "\n"))

# Legal frequency rule (CONAMA 357/2005 Art. 15)
conama_freq_check(df, classe = "2", by = "ponto")
```

---

## Main functions

**Water quality indices**
- `iqa(df, method = "CETESB", na_rm = TRUE, ...)` — Water Quality Index; weighted geometric mean (CETESB/NSF default).
- `iet_carlson(df)` / `iet_lamparelli(df)` — Trophic State Index.
- `nsfwqi(df, na_rm = TRUE, add_status = TRUE)` — NSF WQI with geometric mean aggregation.

**CONAMA compliance and balneability**
- `conama_limits(class)` — limits from CONAMA 357/2005.
- `conama_check(df, class)` — compliance by parameter (`*_ok` columns).
- `conama_freq_check(df, classe, by)` — Art. 15 frequency rule (≥ 80% conformity in ≥ 6 samples/year).
- `balnear_check(df, by)` — balneability classification under CONAMA 274/2000 (Excelente → Imprópria).

**Seasonal analysis and trend**
- `assign_season(df, region)` — classifies samples by hydrological season using regional calendars.
- `compare_seasons(df, param, test)` — statistical comparison between wet and dry seasons.
- `mk_seasonal(df, param, period)` — seasonal Mann-Kendall trend test (Hirsch et al., 1982); returns Z, p-value, tau, and Sen's slope.

**Load and risk**
- `compute_load(df, param, flow_col, unit_out)` — pollutant load (kg/day, t/day, etc.).
- `exceedance_prob(df, param, threshold, direction, by)` — empirical exceedance probability with Wilson CI.

**Multivariate**
- `wq_pca(df, params, color_by)` — PCA with biplot, screeplot, and loadings plot.

**Visualization**
- `plot_iqa()`, `plot_iet()`, `plot_series()`, `plot_box()`, `plot_heatmap()`, `plot_trend()` — return `ggplot` objects.
- `plot_map()`, `plot_map_quality()` — interactive Leaflet maps; `plot_map_quality()` colors sites by IQA/IET/NSF WQI class.

**Data and reports**
- `read_wq(path)` — reads water quality datasets (CSV).
- `validate_wq(df)` — validates/normalizes columns and units.
- `generate_analysis()`, `render_report()` — automated textual and document reporting.

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

> Santos, V. S. (2025). *tikatuwq: Water Quality Assessment and Environmental Compliance in Brazil* (v0.9.0). Zenodo. [https://doi.org/10.5281/zenodo.17407297](https://doi.org/10.5281/zenodo.17407297)

BibTeX entry:

```bibtex
@Manual{Santos2025tikatuwq,
  title  = {tikatuwq: Water Quality Assessment and Environmental Compliance in Brazil},
  author = {Vinicius Saraiva Santos},
  year   = {2025},
  note   = {R package version 0.9.0},
  doi    = {10.5281/zenodo.17407297},
  url    = {https://github.com/tikatuwq/tikatuwq},
}
```

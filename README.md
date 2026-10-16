# 💧 TikatuWQ: An R Package for Water Quality Assessment and Environmental Compliance in Brazil

**Reproducible freshwater monitoring workflows for Brazil, including water quality indices, regulatory compliance checks, and automated reporting.**

**TikatuWQ** is an open-source R package designed to analyze, visualize, and report water quality data according to Brazilian environmental standards.  
It implements the main indices used in the country **IQA/NSFWQI** and **IET (Carlson and Lamparelli)** and provides automated checks for **CONAMA Resolution 357/2005** compliance — including the legal frequency rule (Art. 15).  
The package also includes seasonal analysis, pollutant load computation, exceedance probability, multivariate PCA, trend analysis, data validation, and automatic report generation.

📄 [Ler em Português](https://github.com/tikatuwq/tikatuwq/blob/main/README-pt.md)

```markdown
**Author:** Vinícius Saraiva Santos  
**Institution:** Federal University of Southern Bahia (UFSB) – Graduate Program in Biosystems  
**Research Group:** Tropical Ecosystems Research Center (NuPEcoTropic)  
**Project:** Tikatu – Ecosystem of tools for water quality monitoring and interpretation  
**License:** MIT  
**Software DOI (Zenodo concept DOI, all versions):** https://doi.org/10.5281/zenodo.17407296  
**Manuscript status:** scientific article currently under review in an international journal
```

[![CRAN status](https://www.r-pkg.org/badges/version/tikatuwq)](https://cran.r-project.org/package=tikatuwq)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/tikatuwq)](https://cran.r-project.org/package=tikatuwq)
[![CRAN monthly downloads](https://cranlogs.r-pkg.org/badges/tikatuwq)](https://cran.r-project.org/package=tikatuwq)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17407296.svg)](https://doi.org/10.5281/zenodo.17407296)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/tikatuwq/tikatuwq/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tikatuwq/tikatuwq/actions)
![Manuscript status](https://img.shields.io/badge/manuscript-under_review-yellow)

---

## Scientific Scope

The **tikatuwq** package was designed to support reproducible scientific workflows in freshwater monitoring and environmental assessment. Environmental monitoring datasets often present challenges such as heterogeneous formats, inconsistent parameter names, and the need to interpret regulatory thresholds.

By integrating data validation, environmental indices, regulatory compliance checks, visualization, and automated reporting in a single analytical framework, **tikatuwq** enables researchers to move efficiently from **raw monitoring data to interpretable environmental assessments**.

The package is particularly suited for:

- freshwater monitoring programs  
- environmental impact assessments  
- academic research in aquatic sciences  
- environmental agencies and regulatory analysis  
- long-term environmental datasets  

---

## Included real dataset: Rio Buranhem - INEMA

This package includes a real water quality dataset extracted from INEMA (the Bahia State Environmental Agency) monitoring campaigns conducted in the Rio Buranhem watershed (Porto Seguro, Bahia, Brazil) between 2021 and 2024. The included data provide site IDs, sampling dates, and diverse physicochemical variables measured during field campaigns. See documentation for `wq_demo` for details on columns and usage in analyses.

All main documentation and vignettes use this realistic sample for demonstration and reproducible workflows.

## Basic workflow

A typical analysis workflow using **tikatuwq** follows a reproducible pipeline:

`read_wq → validate_wq → index calculation → regulatory check → visualization → reporting`

```r
library(tikatuwq)
data(wq_demo)
head(wq_demo)

# IQA calculation with the CETESB analytical equations
wq_demo |> validate_wq() |> iqa(allow_partial = TRUE) |> plot_iqa()

# Detailed component-level audit of the 9 CETESB sub-indices
iqa_components(wq_demo)

# Trophic state visualization
wq_demo |> iet_carlson(.keep_ids = TRUE) |> plot_iet(method = "carlson")

# Seasonal analysis
wq_demo |>
  assign_season(region = "bahia") |>
  compare_seasons(param = "turbidez", by = "ponto")
```

---

### 📚 Project and Institutional Context

The **TikatuWQ** package was developed by **Vinícius Saraiva Santos** (author and maintainer)  
as part of the **Tikatu Research Project**, conducted within the **Nucleus for Research in Tropical Ecosystems – NuPEcoTropic**, a research group linked to the **Federal University of Southern Bahia (UFSB)** and coordinated by **Prof. Dr. Fabrício Berton Zanchi**.  

This work was carried out as part of the activities of the **Postgraduate Program in Biosystems (PPG Biossistemas)** at UFSB, under the supervision of **Prof. Dr. Fabrício Berton Zanchi**.  

The **Tikatu Project**, developed and coordinated by **Vinícius Saraiva Santos**, integrates research focused on environmental monitoring and modeling.

---

## 🆕 News

### 🆕 What's new in v0.11.0 (current)

**IQA results change for some inputs.** The pH (8.5–10), BOD (5–15 mg/L) and
total nitrogen (10–60 mg/L) sub-index equations were corrected to remove
discontinuities found in v0.10.0, after comparison with the curves in CETESB
Appendix D (2023, Figure 1). CETESB publishes the sub-indices only as graphical
curves, so the package equations are analytical approximations of them. See `NEWS.md` and
`system.file("audit", "IQA_AUDITORIA.md", package = "tikatuwq")` for the
before/after comparison and the list of open items.

- `classify_iqa()` uses `classification` (as `iqa()` does); `scheme` is deprecated.
- `classify_iqa()` offers the scales `"cetesb"`, `"igam"` and `"ana"` (same cuts as `"cetesb"`); they change only the labels, never the IQA value. In `iqa()` the scale follows `method`; the `classification` argument of `iqa()` was removed (breaking change from 0.10.0): use `classify_iqa()`.
- `iqa(df, method = "IGAM")` computes the IGAM (Minas Gerais) IQA with its own engine (see below).
- `iqa_methods()` lists the calculation engines with their scope of validation.
- `compare_iqa(df, methods = c("CETESB", "IGAM"))` compares the two engines on paired samples.

#### IQA-IGAM: implementation and validation

`tikatuwq` implements a documented computational reproduction of the IQA used
by the Instituto Mineiro de Gestão das Águas (IGAM). It is based on the SCQA
(2005), IGAM's institutional documentation and the academic literature.

- **Turbidity:** the sub-index uses the coefficient `1.5` in the trigonometric
  term, as recorded by Bucci (2011, p. 51) and Nunes (2008, Table 10). Copies
  of the SCQA print `15`.
- **Results:** the choice is supported by reproducing IGAM's published results.
  All **49 records** from three campaigns in the Pampulha sub-basin
  (2017–2018) match the published value after rounding to one decimal place.
  The largest unrounded absolute error is 0.048876. Fifteen of these records
  were used in the investigation; the other 34 were checked with
  pre-registered criteria.
- **Data not redistributed:** the 49 historical records are IGAM data and do
  not ship with the package. Only aggregate metrics are documented, with the
  sources and the reproduction procedure. The package tests use synthetic
  data.
- **Not endorsed:** the implementation is **not endorsed by IGAM**.
- **Not covered:** other basins, nitrate above the detection limit, and IGAM's
  rules for censored values, missing chloride and rounding.
- **Inputs:** IGAM needs chloride (for oxygen saturation) and an explicit
  nitrate unit (`nitrate_unit = "N"` or `"NO3"`). Total phosphorus (`p_total`)
  needs `phosphorus_basis`.

Equations, sources (with pages), input requirements, validation metrics and
the reproduction procedure are in `system.file("methodology", "IGAM_VALIDATION.md", package = "tikatuwq")`.

```r
# Synthetic sample; nitrate in mg/L N and total phosphorus in mg/L P are declared
g <- data.frame(od = 7, temperatura = 25, cloreto = 10, e_coli = 100, ph = 7.3,
                dbo = 2, nitrato = 1, p_total = 0.1, turbidez = 5, solidos_totais = 100)
iqa(g, method = "IGAM", altitude_m = 0, nitrate_unit = "N", phosphorus_basis = "P")
#> IQA 75.5 (Bom)
```

Comparing both engines on the same samples (illustrative data; only rows valid
for both methods enter the statistics). The methods do not use the same
parameters: CETESB uses total nitrogen and total phosphorus, while IGAM uses
nitrate, total phosphate and chloride. Their classes also have different cut-offs, so
differences reflect methodology, not measurement error:

```r
d <- data.frame(
  ponto = c("P1", "P2", "P3", "P4"),
  od = c(7, 5, 3, 6), temperatura = 24, cloreto = c(10, 30, 60, NA),
  coliformes = c(100, 2000, 20000, 500), ph = c(7.2, 7.5, 7, 7.4),
  dbo = c(2, 5, 15, 3), nt_total = c(1, 3, 8, 2), nitrato = c(0.5, 1, 2, 0.8),
  p_total = c(0.05, 0.2, 0.8, 0.1), turbidez = c(5, 20, 60, 10),
  solidos_totais = c(100, 200, 400, 150)
)
cmp <- compare_iqa(d, methods = c("CETESB", "IGAM"), id_cols = "ponto",
                   phosphorus_basis = "P", nitrate_unit = "N", altitude_m = 800)
cmp[, c("ponto", "IQA_CETESB", "IQA_IGAM", "par_valido", "motivo_IGAM")]
attr(cmp, "summary")            # paired statistics (3 valid pairs here)
attr(cmp, "class_transitions")  # CETESB class -> IGAM class (different scales)
```
- `iqa(details = TRUE)` reports the effective weights actually used, including with custom `pesos` and `allow_partial = TRUE`.
- `delta_temperature` is now forwarded; invalid physical values become `NA` with a warning.

### 🆕 What's new in v0.10.0

**🔬 Scientific & Metrological Audit of the Brazilian IQA (CETESB/INEMA)**

- **Analytical Equations**: `iqa()` implements piecewise analytical equations for all 9 CETESB sub-indices ($Q_1$ to $Q_9$), including altitude- and temperature-corrected DO saturation ($C_s$). *Note: the 2026 audit found errors in some pieces of these equations (fixed in development).*
- **Stoichiometric Phosphorus Conversion**: Automatic conversion of total elemental phosphorus to phosphate ($\text{P}_{\text{total}} \times 3.066 = \text{PO}_4$) when `phosphorus_basis = "P"`.
- **E. coli Factor ($1.25\times$)**: Native support for `microbial_type = "e_coli"`, applying a $1.25\times$ conversion factor (CETESB Appendix D 2023, p. 5).
- **Strict Separation of Total Solids vs. TDS**: Total Dissolved Solids (`tds`) is no longer accepted as an interchangeable alias for Total Solids (`solidos_totais`/`residuo_total`) in strict CETESB mode, preventing systematic overestimation of water quality.
- **Component-Level Audit Layer**: New exported function `iqa_components()` and `iqa(..., details = TRUE)` returning raw values, analytical sub-indices ($Q_i$), official weights ($W_i$), and weighted factors ($Q_i^{W_i}$) for full scientific reproducibility.
- **Official CETESB Qualitative Ranges**: `classify_iqa()` updated to official CETESB rating thresholds: *Péssima* ($\le 19$), *Ruim* ($19\text{--}36$), *Regular* ($36\text{--}51$), *Boa* ($51\text{--}79$), *Ótima* ($> 79$).
- **Aquatic Environment Context for CONAMA**: `conama_check()` now accepts `environment = c("all", "lotic", "lentic", "intermediate")` for automatic selection of regulatory phosphorus thresholds and pH-conditioned ammonia limits.
- **INEMA example data**: Test case built from INEMA monitoring data from Rio Buranhém (2024 Campaign 3). It checks the class and a plausible range; it does not compare against IQA values published by INEMA.

✔️ `R CMD check --as-cran`: **0 errors | 0 warnings | 0 notes**  
✔️ **299 automated unit tests passing**  
✔️ Compatible with CRAN, Windows, Linux, and macOS

---

### 🆕 What's new in v0.9.0

**⚠️ Breaking change — IQA now uses the correct weighted geometric mean**

`iqa()` now defaults to `method = "CETESB"`, which computes the weighted geometric mean `∏(Qi^Wi)` as specified by CETESB and the original NSF WQI formulation (Brown et al., 1970). The previous (incorrect) arithmetic mean behavior is preserved via `method = "NSF_approx"`. Users relying on the default will see more accurate — and generally lower — IQA values.

**New functions in v0.9.0:**

- `conama_freq_check()` — implements the legal frequency rule of CONAMA 357/2005 Art. 15: a parameter is considered in conformity only when ≥ 80% of at least 6 samples per year are within limits.
- `assign_season()` — classifies each sample as `"chuvoso"` (wet) or `"seco"` (dry) based on regional Brazilian hydrological calendars.
- `compare_seasons()` — compares water quality parameters between wet and dry seasons.
- `plot_iet()` — bar chart for the Trophic State Index with color-coded trophic classes.
- `compute_load()` — computes pollutant load (kg/day, t/day, kg/year, g/s).
- `exceedance_prob()` — empirical exceedance probability with Wilson confidence intervals.
- `wq_pca()` — PCA wrapper with biplot, screeplot, and loadings plots.
- `nsfwqi()` — updated with geometric mean aggregation and multilingual status labels.

---

## Installation

```r
## Install the released version from CRAN:

install.packages("tikatuwq")

## Installation via GitHub

install.packages("remotes")

# development version
remotes::install_github("tikatuwq/tikatuwq", dependencies = TRUE)

# a specific release by tag (the latest tag on GitHub is v0.8.2; for 0.10.0 use CRAN)
remotes::install_github("tikatuwq/tikatuwq@v0.8.2", build_vignettes = TRUE)
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

### How to cite

If you use **tikatuwq** in your research, please cite it as follows:

> Santos, V. S. (2026). *tikatuwq: Water Quality Assessment and Environmental Compliance in Brazil* (R package version 0.11.0). Zenodo (concept DOI, all versions). [https://doi.org/10.5281/zenodo.17407296](https://doi.org/10.5281/zenodo.17407296)

BibTeX entry:

```bibtex
@Manual{Santos2026tikatuwq,
  title  = {tikatuwq: Water Quality Assessment and Environmental Compliance in Brazil},
  author = {Vinicius Saraiva Santos},
  year   = {2026},
  note   = {R package version 0.11.0},
  doi    = {10.5281/zenodo.17407296},
  url    = {https://github.com/tikatuwq/tikatuwq},
}
```

The DOI is the Zenodo *concept* DOI, which resolves to the latest deposited version (currently v0.7.0); version 0.11.0 has no deposit of its own yet. `citation("tikatuwq")` gives the same reference.

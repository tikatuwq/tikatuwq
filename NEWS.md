# tikatuwq 0.9.0

## Breaking changes

- `iqa()` now defaults to the correct **weighted geometric mean** (`method = "CETESB"`)
  as specified by CETESB and the original NSF WQI (Brown et al., 1970).
  The previous arithmetic mean behaviour is preserved via `method = "NSF_approx"`.
  Existing code using `iqa()` without specifying `method` will produce different
  (more accurate) IQA values. (#P0)

## New functions

- `mk_seasonal()`: seasonal Mann-Kendall trend test (Hirsch, Slack & Smith,
  1982). Splits the time series by season (monthly or wet/dry via
  `assign_season()`), computes S and VAR(S) per season, aggregates, and
  returns Z, p-value, Kendall tau, and Sen's slope (units/year) per group.
  Removes the seasonal bias that inflates false positives in standard
  Mann-Kendall tests applied to tropical rivers. (#P6)
- `plot_map_quality()`: interactive Leaflet map coloring sampling sites by
  IQA, IET (Carlson or Lamparelli), or NSF WQI value. Uses the standard
  classification palettes for each index and shows class and value in the
  popup. Extends `plot_map()` with index-aware choropleth logic. (#P12)
- `balnear_check()`: classifies sampling sites for primary-contact
  recreation (swimming) under CONAMA Resolution 274/2000 (Excelente,
  Muito Boa, Satisfatória, Imprópria) based on the 80% rule over the last
  5 collections. Supports thermotolerant coliforms and E. coli; flags
  sites with insufficient samples. (#P16)
- `conama_freq_check()`: implements the legal frequency rule of CONAMA 357/2005
  Art. 15 — conformity assessed as ≥80% of samples within limits, requiring
  at least 6 samples per year. Returns a tibble with per-point, per-year,
  per-parameter compliance status. (#P1)
- `assign_season()`: adds a hydrological season column (`"chuvoso"` / `"seco"`)
  to a data frame, based on regional Brazilian calendars (Sudeste, Nordeste,
  Norte, Sul, Centro-Oeste, Bahia) or a custom month vector. (#P3)
- `compare_seasons()`: compares a water quality parameter between wet and dry
  seasons using Wilcoxon, t-test, or Kruskal-Wallis; returns descriptive
  statistics, test results, and optionally a `ggplot` boxplot. (#P3)
- `plot_iet()`: bar chart (vertical or horizontal) for the Trophic State Index
  with color-coded trophic classes, supporting both Carlson (1977) and
  Lamparelli (2004) classification schemes. (#P4)
- `compute_load()`: computes pollutant load as concentration × flow × unit
  factor; supports kg/day, t/day, kg/year, and g/s outputs. (#P7)
- `exceedance_prob()`: estimates empirical exceedance probability with Wilson
  confidence interval, by group. (#P8)
- `wq_pca()`: PCA wrapper around `stats::prcomp()` with automatic column
  selection, biplot, screeplot, and loadings plot as `ggplot` attributes. (#P15)
- `nsfwqi()`: updated to use weighted geometric mean (consistent with CETESB
  IQA fix), adds `add_status` and `locale` arguments, removes prototype
  language. (#P5)

## Data

- `inst/extdata/conama_limits.csv` expanded from ~38 to ~116 rows, adding
  nitrogen species (NO3, NO2, NH3 with pH-conditional limits), inorganic ions
  (fluorides, chlorides, sulfates), organic pollutants (phenols, surfactants),
  and 14 heavy metals / trace elements for Classes 1–3. (#P2)

## Internal

- `R/testando.R`: replaced empty file with documented placeholder comment.
- `R/wq_buranhem.R`: removed duplicate `@docType` tag that caused a `R CMD check`
  WARNING.
- `DESCRIPTION`: version bumped to 0.9.0; `tools` added to Imports.
- Tests added for all new modules (`test-iqa-geometric.R`, `test-conama_freq.R`,
  `test-seasonal.R`, `test-load_exceedance.R`, `test-plot_iet.R`,
  `test-wq_pca.R`).

# tikatuwq 0.8.2

- **CRAN maintenance**: Patch release for CRAN compliance. Fixed example in `plot_map()` to use internal dataset `wq_demo` instead of external file reference. All examples and tests now comply with CRAN policies (no local file dependencies, write only to tempdir). No API changes.

# tikatuwq 0.8.1

- **Fix (CRAN compliance)**: `render_report()` now copies the template to a temporary directory before rendering to comply with CRAN filesystem policies. All intermediate files generated during rendering are written only to `output_dir` (default `tempdir()`), never to the package installation directory. Tests for `render_report()` are now skipped on CRAN (`skip_on_cran()`) and use `withr::local_tempdir()` for proper cleanup.

# tikatuwq 0.8.0

- **Data and documentation update**: The `wq_demo` example dataset is now a subset of real data (from BURANHEM river monitoring, 4 points, years 2020–2024, 20 rows). Includes extra columns: `rio`, `lat`, and `lon`.
- All examples and vignettes now use real, representative dataset (greater realism and reproducibility).
- Documentation (`R/data_wq_demo.R`, inst/extdata/wq_demo_README.txt, vignettes, README) updated to reflect the change.
- No API breakage; all functions and test expectations remain valid for the new dataset structure and size.

# tikatuwq 0.7.3

- Funções `iet_carlson()` e `iet_lamparelli()` agora aceitam `data.frame` com `.keep_ids`.
- Conversão automática de formato BR ("," / "<" / ">") e de `p_total` (mg/L) → `tp` (µg/L).
- `iqa()` mais robusto: alias `temp` para `temperatura`, numificação segura e reponderação com `na_rm=TRUE`.
- Campos identificadores (rio, ponto, data, lat, lon) agora são automaticamente reconhecidos e preservados via `.keep_ids`.

# tikatuwq 0.7.2

Changes in this version:
- Fixed incoming NOTE detected in pre-tests:
  - Removed non-standard fields (`DOI`, `Citation`) from DESCRIPTION.
  - Updated `inst/CITATION` to use the modern `bibentry()` format instead of the deprecated `citEntry()`.
- Minor internal documentation updates (no API or functional changes).

# tikatuwq 0.7.1

- **Title** field adjusted per CRAN request: now *"Water Quality Assessment and Environmental Compliance in Brazil"*.
- Added **Zenodo DOI** and formal citation entry (`citation("tikatuwq")`).
- Updated **README** (EN/PT) with citation, DOI badge, and institutional context.
- No changes in functions or internal code.

# tikatuwq 0.7.0

- New functions `param_analysis()` and `param_analysis_multi()`:
  - Enable detailed analysis of specific parameters by point, river, or group of points.
  - Support cross-comparisons (multiple parameters at one point or multiple points for the same parameter).
  - Include simple temporal trend detection and basic statistical summary.
  - Return standardized data frames compatible with visualization and reporting tools.
- Automated tests for new modules (`test-param_analysis.R`, `test-param_analysis_multi.R`).
- Code updated to meet CRAN best practices (all checks passed 100% cleanly).
- Minor stability improvements in `plot_trend()` and `plot_map()` (controlled messages).

# tikatuwq 0.6.2

- Fix: resolved codoc WARNING for `generate_analysis()` (docs matched to code).
- No functional code changes.

# tikatuwq 0.6.1

- Maintenance update requested by CRAN.
- Fixed relative link `README-pt.md`, now using absolute HTTPS URL.
- No functional changes in code.

# tikatuwq 0.6.0

- New function `plot_trend()`:
  - Visualize time series for parameters (e.g., turbidity, OD, IQA).
  - Add trend lines by group/point using three methods:
    - **Theil–Sen** (robust to outliers),
    - **OLS** (ordinary least squares),
    - **LOESS** (smoothed curve).
  - Supports faceting by river/point and customizable number of samples.
  - Returns a `ggplot` object ready for plotting.
- Updated documentation for `plot_trend()` with practical examples.
- Updated pkgdown site with dedicated section for trend visualization.

# tikatuwq 0.5.1

- Fix: replaced broken URLs in help pages.

# tikatuwq 0.5.0

- New: `plot_map()` — interactive map of sampling points using **Leaflet** (optional dependency via Suggests).
- Examples wrapped in `if (interactive())` / `\donttest{}` to avoid network calls on CRAN.
- Tests: added `skip_on_cran()` and `skip_if_not_installed("leaflet")` guards for map tests.
- Encoding hardening: replaced non-ASCII characters with `\u` escapes.
- Dependencies: removed unused `htmltools` from Imports.
- Docs: roxygen updates; added function to “Visualizations” reference.
- No breaking changes; existing public API unchanged.

# tikatuwq 0.4.6

- DESCRIPTION rewritten in English-only to avoid CRAN “Possibly misspelled words” NOTE.
- Fixed DESCRIPTION URLs (Lamparelli 2004, CONAMA 357/2005).
- Updated spell-check list (`inst/WORDLIST`).
- Added expanded acronyms and DOIs/URLs in DESCRIPTION.
- Documented `@return` for all exported functions.
- Maintained MIT license text (per CRAN policy).
- Minor internal refactor: `conama_limits()` now accepts `class` argument for filtering.

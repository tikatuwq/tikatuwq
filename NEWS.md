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

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

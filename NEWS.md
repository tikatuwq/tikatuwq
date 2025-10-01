# tikatuwq 0.5.1
- Fix: replace/repair broken URLs in package help page.

# tikatuwq 0.5.0 (2025-09-30)
- New: `plot_map()` — interactive map of sampling points using Leaflet (optional dependency via Suggests).
- Examples wrapped in `if (interactive())` / `\donttest{}` to avoid network calls on CRAN.
- Tests: add `skip_on_cran()` and `skip_if_not_installed("leaflet")` guard for map tests.
- Encoding hardening: replace non-ASCII characters in R code strings with `\u` escapes to silence checks.
- Dependencies: remove unused `htmltools` from Imports (Leaflet remains in Suggests).
- Docs: roxygen update; add function to pkgdown “Visualizations” reference.
- Chore: move non-code markdown files out of `R/` to `inst/notes/` to keep build clean.
- No breaking changes; existing public API unchanged.

# tikatuwq 0.4.6
- DESCRIPTION rewritten to English-only to avoid CRAN “Possibly misspelled words” NOTE.
- Fix DESCRIPTION URLs per CRAN (Lamparelli 2004, CONAMA 357/2005).
- Spell-check workflow: updated `inst/WORDLIST` (Portuguese terms & acronyms).
- Expanded all acronyms in DESCRIPTION (WQI ↔ IQA, TSI ↔ IET, CONAMA, NSF).
- Added references with `<doi:...>` and `<https:...>` in DESCRIPTION.
- Documented `\value` sections via `@return` for all exported functions:
  - `conama_limits()`: tibble/data frame of thresholds.
  - `iqa()`: tibble with IQA scores and qualitative class.
  - `plot_box()`, `plot_heatmap()`, `plot_iqa()`, `plot_series()`: ggplot objects.
- Kept LICENSE as plain MIT text (per CRAN policy).
- Minor internal refactor: `conama_limits()` now accepts `class` argument for filtering.
- Version bump to 0.4.6 for CRAN submission.

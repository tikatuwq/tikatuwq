# tikatuwq 0.4.6
- DESCRIPTION rewritten to English-only to avoid CRAN “Possibly misspelled words” NOTE.
- Fix DESCRIPTION URLs per CRAN (Lamparelli 2004, CONAMA 357/2005).
- Spell-check workflow: updated inst/WORDLIST (Portuguese terms & acronyms).



- Expanded all acronyms in DESCRIPTION (WQI ↔ IQA, TSI ↔ IET, CONAMA, NSF).
- Added references with <doi:...> and <https:...> in DESCRIPTION.
- Documented \value sections via @return for all exported functions:
  - `conama_limits()`: tibble/data frame of thresholds.
  - `iqa()`: tibble with IQA scores and qualitative class.
  - `plot_box()`, `plot_heatmap()`, `plot_iqa()`, `plot_series()`: ggplot objects.
- Kept LICENSE as plain MIT text (per CRAN policy).
- Minor internal refactor: `conama_limits()` now accepts `class` argument for filtering.
- Bumped version to 0.4.4 for CRAN resubmission.

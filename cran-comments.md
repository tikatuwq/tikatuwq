## Resubmission (0.4.6)
- DESCRIPTION now English-only (removed Portuguese words and diacritics) to avoid CRAN “Possibly misspelled words” NOTE.

- Fixed two DESCRIPTION URLs flagged as 404 (Lamparelli 2004, CONAMA 357/2005).
- Updated inst/WORDLIST to silence false positives (package documented in Portuguese).
- No code changes; local R CMD check --as-cran clean.

This is a resubmission after CRAN feedback.

- Expanded acronyms in DESCRIPTION (WQI ↔ IQA; TSI ↔ IET; CONAMA; NSF).
- Added references using `<doi:...>` and `<https:...>` in DESCRIPTION (Carlson 1977 via DOI; Lamparelli 2004, NSF WQI and CONAMA 357/2005 via HTTPS).
- Added `\value` sections via `@return` for exported functions:
  `conama_limits()`, `iqa()`, `plot_box()`, `plot_heatmap()`, `plot_iqa()`, `plot_series()`.
- Documented dataset `wq_demo` and recompressed data with xz (`LazyDataCompression: xz`).

## Test environments
- Local: Windows 11, R 4.5.1 (ucrt)
- win-builder: R-devel, R-release (OK)
- R-hub: Linux/macOS/Windows (OK)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Reverse dependencies
None.

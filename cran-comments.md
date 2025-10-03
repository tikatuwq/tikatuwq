## CRAN submission v0.6.0

This is a new release of **tikatuwq**, following v0.5.0 currently on CRAN.

### Test environments
- Windows 11 (local): R 4.5.1 (ucrt), no errors/warnings/notes
- macOS (local via R-hub): R-release, no errors/warnings/notes
- Ubuntu 22.04 (GitHub Actions): R-release, no errors/warnings/notes

### R CMD check results
0 errors | 0 warnings | 0 notes

### Changes since last CRAN release (v0.5.0)
- Added new function `plot_trend()` for temporal trend analysis:
  - Supports **Theil-Sen**, **OLS**, and **LOESS** methods.
  - Allows faceting by river/site and point customization.
  - Returns a `ggplot` object ready for visualization or inclusion in reports.
- Updated documentation, examples, and pkgdown site.
- Updated README with latest release notes.

### Notes
- No reverse dependency issues detected.
- This update introduces a new visualization function, but does not break existing functionality.

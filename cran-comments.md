## Test environments
- Windows 11, R 4.5.1 (x86_64, ucrt)
- macOS 14 (local), R 4.3+
- Ubuntu 22.04 (CI), R 4.3+

## R CMD check results
0 errors | 0 warnings | 0 notes

## Changes
- v0.8.0 (minor, backward-compatible):
  * `wq_demo` now points to a real subset of monitoring data (INEMA, Rio Buranhem, Porto Seguro-BA, 2021–2024), 20 rows and 14 columns (including `rio`, `lat`, `lon`).
  * All examples and vignettes updated to use the real dataset, improving reproducibility and documentation value.
  * No API break; functions and test expectations remain valid.

## Reverse dependencies
- No reverse dependencies at this time.

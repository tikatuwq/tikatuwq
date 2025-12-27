## Test environments
- Windows 11, R 4.5.1 (x86_64, ucrt)
- macOS 14 (local), R 4.3+
- Ubuntu 22.04 (CI), R 4.3+

## R CMD check results
0 errors | 0 warnings | 0 notes

## Changes
- v0.8.2 (patch, CRAN maintenance):
  * Fixed example in `plot_map()` to use internal dataset `wq_demo` instead of external file reference (`dataset-real.csv`).
  * All examples and tests now comply with CRAN policies (no local file dependencies, write only to tempdir).
  * No API changes; backward compatible.

- v0.8.1 (patch, CRAN compliance fix):
  * **Fixed filesystem write errors on CRAN Debian/Linux**: `render_report()` now copies the R Markdown template to a temporary directory before rendering, ensuring all intermediate files are written only to `output_dir` (default `tempdir()`), never to the read-only package installation directory. This resolves the "cannot open the connection" errors during `rmarkdown::render()` → `knitr::knit()` → `xfun::write_utf8()`.
  * Tests in `test-render_report.R` now use `skip_on_cran()` and `withr::local_tempdir()` for proper cleanup and CRAN compliance.
  * Added `withr` to Suggests for test infrastructure.
  * No API changes; backward compatible.

- v0.8.0 (minor, backward-compatible):
  * `wq_demo` now points to a real subset of monitoring data (INEMA, Rio Buranhem, Porto Seguro-BA, 2021–2024), 20 rows and 14 columns (including `rio`, `lat`, `lon`).
  * All examples and vignettes updated to use the real dataset, improving reproducibility and documentation value.
  * No API break; functions and test expectations remain valid.

## Reverse dependencies
- No reverse dependencies at this time.

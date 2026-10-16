## Submission of tikatuwq 0.11.0 (DRAFT — not submitted)

This release corrects scientific errors found in an audit of the water quality
indices, adds an IGAM water quality index engine, and simplifies the `iqa()`
interface.

* **IQA (CETESB) sub-index equations.** Corrected pieces of the BOD (5-15 mg/L),
  total nitrogen (10-60 mg/L) and pH (8.5-10) curves. In 0.10.0 they used the
  wrong logarithm/exponential base and could change IQA values by up to about
  60 points. The equations are documented as analytical approximations of the
  graphical CETESB curves, with sources (`inst/audit/`).
* **Trophic state indices.** `iet_lamparelli()` now uses the Lamparelli (2004)
  equations, and `iet_carlson()` the natural logarithm of Carlson (1977).
* **New `iqa(method = "IGAM")`.** It is an engine for the IQA of the Minas
  Gerais water agency (IGAM), independent of the CETESB engine.
  * It is a documented computational reproduction, not endorsed by IGAM.
  * It reproduces 49/49 published IGAM values from the Pampulha sub-basin
    (2017-2018).
  * It requires explicit units: the nitrate unit, the phosphorus basis and
    chloride. It does not assume defaults.
  * Methodology note: `inst/methodology/IGAM_VALIDATION.md`.
* **New `iqa_methods()` and `compare_iqa()`.** `compare_iqa()` makes paired
  comparisons between engines.
* **Breaking change.** The `classification` argument of `iqa()` was removed;
  the scale now follows `method`, and `classify_iqa()` is unchanged. Passing
  `classification` (or using the 0.10.0 positional form) gives an error that
  explains the migration.
* **`nsfwqi()`.** It is documented as an approximation, and strict mode now
  honours its documented contract.

Results of previously published analyses may change. This is described in
`NEWS.md` and in `inst/audit/`.

## R CMD check results

* **Local:** Windows 11 x64 (build 26200), R 4.5.1 (ucrt), pandoc 3.8.3.
* **Command:** `R CMD check --as-cran --no-manual` on the built tarball
  (2026-10-16), including the CRAN incoming feasibility and URL checks.
* **Result:** 0 errors | 0 warnings | 0 notes.

## Not yet done (required before submission)

* win-builder (R-devel and R-release) and macOS checks: **not run**.
* Check with the PDF manual: **not run** (LaTeX is not available on the local
  machine).
* Reverse dependencies: none known; to be confirmed with `revdepcheck` or the
  CRAN reverse-dependency list.

## Test results

`devtools::test()` (local): 37 test files, 182 tests, 801 expectations,
0 failures, 0 errors, 5 skipped.

* 3 tests in `test-render_report.R` are skipped when pandoc is not on the
  path. With pandoc available, 2 of them run and pass; the third is skipped by
  design when rmarkdown is installed.
* 1 test in `test-plot_map_quality.R` is skipped by design when leaflet is
  installed.
* 1 test in `test-mk_seasonal.R` is recorded as empty: its expectation is
  conditional and its synthetic series does not trigger it.

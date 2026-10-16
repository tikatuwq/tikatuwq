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

All results below are for the submitted tarball (release candidate 2,
SHA-256 `0775fc48e6659c7698f7043cbcb9a906c56482219d45a5196373cc4f282b793d`),
checked on 2026-10-16. The remote services received this exact file; the
SHA-256 was checked before each upload.

### Windows 11 x64 (local)

* **Environment:** R 4.5.1 (ucrt), Rtools 4.5, pandoc 3.8.3, TinyTeX.
* **Command:** `R CMD check --as-cran`, **with** the PDF manual.
* **Result:** 0 errors | 0 warnings | 0 notes.

### Ubuntu 26.04 LTS (WSL2, local)

* **Environment:** R 4.5.3 (conda-forge), locale C.UTF-8, pandoc 3.12.1.
* **Command:** `R CMD check --as-cran --no-manual` (no LaTeX on this machine;
  the PDF manual was checked on Windows and macOS).
* **Result:** 0 errors | 0 warnings | 0 notes.

### macOS (mac builder)

* **Environment:** R 4.6.1 Patched (2026-07-27 r90311), aarch64-apple-darwin23,
  macOS Tahoe 26.6, Apple M1.
* **Result:** Status OK (0 errors | 0 warnings | 0 notes), including the PDF
  manual.

### win-builder (R-devel and R-release)

* **PENDING:** submitted on 2026-10-16 at 23:16 UTC; results not yet
  received. To be filled in with the real results before submission.
* An earlier candidate, identical except for README.md and NEWS.md, gave
  0 errors | 0 warnings | 1 note on both R-devel (2026-10-14 r90673 ucrt) and
  R-release (R 4.6.1). The note was in "checking CRAN incoming feasibility":

  ```
  Possibly misspelled words in DESCRIPTION:
    Gerais (25:62)
    Minas (25:56)
  ```

  "Minas Gerais" is the name of a Brazilian state, spelled correctly. It
  identifies the state water agency (IGAM) whose index the package
  reproduces.

## Reverse dependencies

There are none on CRAN. This was checked on 2026-10-16 with
`tools::package_dependencies(reverse = TRUE)`.

## Test results

`devtools::test()` (Windows): 37 test files, 181 tests, 806 expectations,
0 failures, 0 errors, 2 skipped by design:
* the leaflet fallback test, skipped when leaflet is installed;
* one `render_report` test, skipped when rmarkdown is installed.

Inside `R CMD check` (CRAN mode, Windows and Linux): 736 passed, 16 skipped
(15 by `skip_on_cran()`, plus the leaflet fallback test), 0 failures.

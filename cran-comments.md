# CRAN submission — tikatuwq 0.7.0

## Test environments
* Windows 11, R 4.5.1 (local): OK  
* win-builder (R-devel): OK  
* Ubuntu 22.04 (GitHub Actions, R-release/R-devel): OK  
* macOS (GitHub Actions, R-release): OK  

## R CMD check results
✔ 0 errors | ✔ 0 warnings | ✔ 0 notes  
(Validated locally, on win-builder, and via CI workflows)

## Changes since v0.6.2
### New features
- Added `param_analysis()` and `param_analysis_multi()` for flexible parameter-wise analysis across sites and sampling points, including descriptive summaries and trend detection.

### Improvements
- Updated `plot_trend()` to use tidy-evaluation (`aes()` + `.data`) — replacing deprecated `aes_string()`.
- Controlled coordinate warnings in `plot_map()`; new test coverage ensures reproducible outputs.
- General documentation and reference updates across all new features.

### Documentation
- Updated **README (EN/PT)**, **NEWS.md**, and **pkgdown** reference index.
- Full help pages added for all new functions, with examples validated on R 4.5.1.

## Reverse dependencies
- No reverse dependencies to check.

## Submission frequency
This is a **minor feature release** following CRAN policies.  
Previous version (v0.6.2) passed checks with no issues; submission delayed by >14 days.

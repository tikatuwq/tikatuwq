# CRAN submission — tikatuwq 0.7.0

## Test environments
* Windows 11, R 4.4.x (local): OK  
* win-builder (R-devel): OK  
* Ubuntu 22.04 (GHA) R-release/R-devel: OK  
* macOS (GHA) R-release: OK  

## R CMD check results
- 0 errors | 0 warnings | 0 notes (local and CI)

## Changes since 0.6.2
- New features:
  - `param_analysis()` and `param_analysis_multi()` for flexible parameter-wise analyses across sites/points,
    including descriptive summaries and simple trend detection.
- Improvements:
  - `plot_trend()` migrated from `aes_string()` to tidy-eval (`aes()` + `.data`) to silence deprecations.
  - `plot_map()` warnings now controlled and covered by tests.
- Documentation:
  - Updated README (EN/PT), NEWS, and pkgdown reference index.
  - Full reference pages for new functions (pkgdown site updated).

## Reverse dependencies
- No strong reverse dependencies.

## Submission frequency
- We are aware of CRAN’s submission frequency policy and will comply.

## Test environments
* Windows 11, R 4.4.x (local): OK  
* win-builder (R-devel): OK  
* Ubuntu 22.04 (GHA) R-release/R-devel: OK  
* macOS (GHA) R-release: OK  

## R CMD check results
0 ERRORs | 0 WARNINGs | 0 NOTEs

## Submission notes
Patch release to fix a codoc WARNING reported by CRAN for the previous version (0.6.1):
* Removed undocumented/unused parameters (`id_cols`, `filter`) from the roxygen header of `generate_analysis()` to match the current function definition.

No functional or behavioral changes were made to the package.
This submission was made in response to a CRAN request.

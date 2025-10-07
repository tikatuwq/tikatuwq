## Test environments
* Windows 11, R 4.4.x (local): OK
* win-builder (R-devel): OK
* Ubuntu 22.04 (GHA) R-release/R-devel: OK
* macOS (GHA) R-release: OK

## R CMD check results
0 ERRORs | 0 WARNINGs | 0 NOTEs

## Submission notes
Maintenance-only update to address the NOTE observed by CRAN:
* Replaced the relative README link `README-pt.md` with an absolute HTTPS URL
  so it resolves correctly outside GitHub/CRAN build contexts.

No functional code changes.

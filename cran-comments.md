## CRAN submission: tikatuwq 0.5.1

### Test environments
- Local: Windows 11 x86_64, R 4.5.1
- GitHub Pages/pkgdown built locally (no network access required by vignettes)
- win-builder: r-devel

### R CMD check results
- Local `R CMD check --as-cran`: **0 errors | 0 warnings | 0 note**

## Changes
- Replaced invalid links in package help; no code changes.

**NOTE (local, Windows only)**
```
checking for future file timestamps ... NOTE
  unable to verify current time
```
This appears to be environment-specific (Windows clock/TMP). It does not reflect a user-facing issue. 
File modification times have been normalized in the working tree; the NOTE may still be reported on some local Windows setups. 
Other platforms are expected to be clean.

### Notes for CRAN
- New version **0.5.0** adds an exported function: `plot_map()` for interactive mapping (Leaflet listed in Imports).
- No breaking changes to the existing API.
- Interactive examples are wrapped in `if (interactive())` / `\donttest{}` to avoid issues on CRAN.
- Vignette loads a bundled demo dataset, with a local CSV fallback; **no network access** is performed.
- Non-ASCII strings in R code were replaced with `\u` escapes to ensure portability across platforms.
- Unused dependency `htmltools` was removed; the package builds and checks cleanly with the current Imports/Suggests.
- URLs and pkgdown site have been validated locally; all links are reachable at build time.

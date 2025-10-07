# tikatuwq 0.6.2
- Fix: resolve codoc WARNING for `generate_analysis` (docs matched to code).
- No functional code changes.

### Novidades v0.6.1
- Atualização de manutenção solicitada pelo CRAN.
- Correção do link relativo `README-pt.md`, agora em URL HTTPS absoluto.
- Nenhuma mudança funcional no código.

# tikatuwq 0.6.0
- Nova função `plot_trend()`:
  - Permite visualizar séries temporais de parâmetros (ex.: turbidez, OD, IQA).
  - Adiciona linhas de tendência por grupo/ponto usando três métodos:
    - **Theil-Sen** (robusto a outliers),
    - **OLS** (regressão linear clássica),
    - **LOESS** (curva suavizada).
  - Suporte a facetas por rio/ponto, personalização de pontos e número mínimo de amostras.
  - Retorna objeto `ggplot`.

- Documentação revisada para `plot_trend()`, incluindo exemplos práticos.
- Atualização do site pkgdown com seção dedicada a gráficos de tendência.


# tikatuwq 0.5.1
- Fix: replace/repair broken URLs in package help page.

# tikatuwq 0.5.0 (2025-09-30)
- New: `plot_map()` — interactive map of sampling points using Leaflet (optional dependency via Suggests).
- Examples wrapped in `if (interactive())` / `\donttest{}` to avoid network calls on CRAN.
- Tests: add `skip_on_cran()` and `skip_if_not_installed("leaflet")` guard for map tests.
- Encoding hardening: replace non-ASCII characters in R code strings with `\u` escapes to silence checks.
- Dependencies: remove unused `htmltools` from Imports (Leaflet remains in Suggests).
- Docs: roxygen update; add function to pkgdown “Visualizations” reference.
- Chore: move non-code markdown files out of `R/` to `inst/notes/` to keep build clean.
- No breaking changes; existing public API unchanged.

# tikatuwq 0.4.6
- DESCRIPTION rewritten to English-only to avoid CRAN “Possibly misspelled words” NOTE.
- Fix DESCRIPTION URLs per CRAN (Lamparelli 2004, CONAMA 357/2005).
- Spell-check workflow: updated `inst/WORDLIST` (Portuguese terms & acronyms).
- Expanded all acronyms in DESCRIPTION (WQI ↔ IQA, TSI ↔ IET, CONAMA, NSF).
- Added references with `<doi:...>` and `<https:...>` in DESCRIPTION.
- Documented `\value` sections via `@return` for all exported functions:
  - `conama_limits()`: tibble/data frame of thresholds.
  - `iqa()`: tibble with IQA scores and qualitative class.
  - `plot_box()`, `plot_heatmap()`, `plot_iqa()`, `plot_series()`: ggplot objects.
- Kept LICENSE as plain MIT text (per CRAN policy).
- Minor internal refactor: `conama_limits()` now accepts `class` argument for filtering.
- Version bump to 0.4.6 for CRAN submission.

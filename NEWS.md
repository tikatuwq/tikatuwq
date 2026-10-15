# tikatuwq 0.11.0

Scientific stabilization of the IQA core and of the trophic state indices.
**IQA and IET values change for some inputs**; see `inst/audit/IQA_AUDITORIA.md`
and `inst/audit/REGRESSAO_0_10_0_0_11_0.csv` for the full before/after
comparison.

CETESB (2023) Appendix D gives the IQA sub-indices only as graphical curves
(Figure 1). The package's piecewise equations come from the CETESB spreadsheet
as transcribed by CPRH (PNMA II) and by von Sperling (2007, p. 255; reproduced
by IBEAS 2015). Weights, fixed out-of-range values, unit conversions and the
classification match the official text exactly; where the two secondary
transcriptions disagree, the choice follows the official curve. An
independent re-implementation agrees with `iqa()` to 1e-14 on 3000 inputs, and
the published IBEAS/von Sperling worked example (IQA = 81) is reproduced.

## Changes in numerical results

- `.qi_cetesb_ph()`, 8.5 < pH <= 9: `10^(-1.1507 pH)` replaced by `exp(-1.1507 pH)`.
  The old form gave qi ~ 0.0002 at pH 8.6. Example: `wq_demo` point
  FBS-BRH-300 on 2023-05-24 moves from 15.58 (Pessima) to 82.55 (Otima).
- `.qi_cetesb_ph()`, 9 < pH <= 10: `50 - 32 (pH - 9)` replaced by `288 - 27 pH`
  (removes the 45 -> 50 jump at pH 9; Figure 1 has no jump there). CPRH prints
  `228 - 27 pH` (an evident typo: it would give qi < 0), while von Sperling via
  IBEAS gives the old form. Effect on the IQA: at most 0.9 point.
  **Maintainer decision pending.**
- `.qi_cetesb_dbo()`, 5 < DBO <= 15 mg/L, and `.qi_cetesb_nt()`, 10 < NT <= 60 mg/L:
  `log10()` replaced by natural `log()`, removing the jumps at 5/15 and 10/60 mg/L.
  IQA drops by up to ~9 points (DBO) and ~15 points (NT) in these ranges.
  Figure 1 confirms the corrected values (e.g. BOD 10 mg/L: ~30, was 73).
  Note: spreadsheets that follow the von Sperling (2007) transcription with
  `log10` (e.g. IBEAS 2015) give higher IQA values in these ranges.
- DO saturation `Cs`: cubic coefficient 0.00005898 corrected to 0.00005896
  (CPRH; von Sperling via IBEAS). Effect below 0.02% of `Cs`.
- `.qi_cetesb_temperatura()`: delta T < -5 C now gives `NA` (qi is "undefined"
  per Figure 1); it returned 93. `temperature_method = "delta"`/`"reference"`
  now warns that the package's delta-T curve does not match the CETESB curve;
  these modes are **experimental** and flagged in `attr(, "iqa_nonstandard")`.
  The default (qi = 94: CPRH and IBEAS "constant of 94 adopted by CETESB";
  consistent with the source's q^w = 1.58) is unchanged.
- `iqa()` now assigns `IQA_status` from the unrounded IQA; rounding to two
  decimals is display only (e.g. 79.004 is Otima although shown as 79.00).
- `iet_lamparelli()`: now uses the Lamparelli (2004) equations given by CETESB
  (Appendix D 2023, p. 10), different for rivers and reservoirs, with total
  phosphorus and chlorophyll-a in ug/L. Until v0.10.0 it used `10 + 10 log10(x)`
  for both environments, which classified almost any sample as
  ultraoligotrophic (e.g. TP = 137 ug/L in a river gave 1.4 instead of 59.5).
  Results now reproduce the class limits of Tables 5 and 6. Secchi depth is
  returned as an auxiliary component and no longer enters `IET_Lamp`, as
  CETESB does not use transparency.
- `iet_carlson()`: natural logarithm instead of `log10` (Carlson 1977 defines
  TSI = 10 (6 - log2 SD); the 14.41/14.42/9.81 coefficients only hold with ln).
  Example: TP = 48 ug/L now gives 60 (was 28.4).
- `generate_analysis()`: the dominant IQA class now uses the CETESB scale of
  `classify_iqa()` instead of a private 25/50/70/90 scale, and the text reports
  how many samples are partial IQA estimates.
- `plot_map_quality()`: auto-detection now recognizes `IET_Lamp`, the column
  returned by `iet_lamparelli()` (it only looked for `IET_Lamparelli`, so
  Lamparelli output was never detected; the corresponding test was silently
  skipped).
- Physically invalid inputs (negative concentrations, pH outside 0-14,
  temperature outside -2..50 C, `Inf`) now become `NA` with a warning instead
  of being silently truncated to 0 (which gave the best possible qi).

## Calculation engines and comparison

- New `iqa_methods()` lists the IQA calculation engines and their validation
  status.
- `method = "IGAM"` is declared in `iqa()` and `iqa_components()` but
  **blocked**: the IGAM sub-index equations (SCQA, SEMAD/PNMA II 2005) could
  not be obtained from a primary source, and the CETESB equations are not
  reused for IGAM (different analytes: nitrate and total phosphate). The call
  fails with an error of class `tikatuwq_method_unavailable` that also
  diagnoses the inputs (e.g. `nt_total` is not used for nitrate). Weights,
  mandatory parameters, missing-data rule, temperature qs = 92 and classes are
  documented in `inst/audit/VALIDACAO_IGAM.md`.
- New `compare_iqa()`: runs each engine on the same rows, keeps row identity
  (`id_cols`), reports eligibility and reasons per method, and computes paired
  statistics (mean/median difference, MAE, RMSE, correlations with >= 3 pairs)
  only on rows valid for all methods. Class differences are reported as a
  transition table, not as label agreement. Unavailable methods make all rows
  ineligible instead of failing.
- `iqa()`: the classification scale now follows `method`. `classification` is
  deprecated (warning); a scale different from the method's own is flagged as
  `"cross_classification"` in `attr(, "iqa_nonstandard")`. New attributes
  `iqa_calculation_method` and `iqa_method_version`. Planned removal of the
  argument: not before 0.13.0. Use `classify_iqa()` for other scales.
- New canonical column `fosfato_total` (total phosphate as PO4), distinct from
  `p_ortofosfato` and `p_total`.
- ANA: no `method = "ANA"` (see `inst/audit/ANA_VS_CETESB.md`);
  `classify_iqa(classification = "ana")` stays.

## nsfwqi()

- Documented as an **approximation by coarse step functions**, not the
  validated NSF WQI (sub-index maximum is 90, so the index never exceeds 90;
  DO in mg/L). New attribute `nsfwqi_validation`.
- With `na_rm = FALSE`, a component without a column now gives `NA`, as
  documented (it was silently dropped with renormalized weights). All 20
  `wq_demo` rows (no nitrate, no total solids) change from a value to `NA` in
  strict mode; partial (`na_rm = TRUE`) values are unchanged
  (`inst/audit/REGRESSAO_NSFWQI_0_11_0.csv`).
- Total suspended solids are no longer used as total solids (warning).

## API

- `classify_iqa()` now uses `classification` (same name as in `iqa()`).
  `scheme` still works but is deprecated (warning); giving both with different
  values is an error. `iqa()` also accepts `scheme` via `...` with the same rules.
- New classification scales `"igam"` (25/50/70/90) and `"ana"`. `"ana"` uses the
  19/36/51/79 cuts shown on the current ANA portal and is an alias of `"cetesb"`.
  `classification` never changes the numeric IQA.
- `classify_iqa()` returns `NA` (with a warning) for values outside [0, 100];
  previously they were classified as the lowest/highest class.
- `iqa()` now forwards `delta_temperature` (it was silently ignored).
  `temperature_method = "delta"` without a delta, or `"reference"` without
  `temperature_reference`, is now an error instead of silently using qi = 94.
- `pesos`: must be a named, finite, non-negative vector of known parameter
  names. Unknown names (e.g. `tds`) were silently ignored before and are now an
  error. With `details = TRUE`, the `wi_*` and `term_*` columns hold the
  **effective** (renormalized) weights, so `prod(qi^wi)` reproduces the IQA.
- `allow_partial = TRUE`: partial rows are listed in `attr(, "iqa_partial_rows")`
  and, with `details = TRUE`, in the new `IQA_partial`/`missing_params` columns.
  This is a package estimate, not an institutional rule.
- `altitude_m` accepts one value per row; values outside -500..9000 m are an error.
- `na_rm` in `iqa()` is deprecated in favour of `allow_partial`. Unknown
  arguments passed through `...` now produce a warning.
- New provenance attributes on `iqa()` output, including for empty input and
  the legacy routes: `iqa_method_reference`, `iqa_weights`, `iqa_custom_weights`,
  `iqa_allow_partial`, `iqa_partial_rows`, `iqa_nonstandard`. `iqa_complete` now
  reports whether every computed row was complete (it used to echo
  `!allow_partial`).

## Documentation

- Removed unverified claims that the equations are "official", "validated" and
  "continuous". Pending items are listed in `inst/audit/IQA_AUDITORIA.md`.
- New sections in `?iqa`: method vs classification, missing data, invalid values.
- `?wq_demo` now states that the dataset has TDS but not total solids, so its
  IQA values are partial estimates (8 parameters), not CETESB IQA values, and
  that it is not an independent benchmark.

# tikatuwq 0.10.0

## Breaking changes and scientific corrections

- **Official CETESB analytical equations**: `iqa()` now implements the official validated continuous piecewise polynomial/exponential equations from CETESB/INEMA rather than discrete/interpolated approximation tables.
- **Total Solids vs TDS**: The canonical 9th component of CETESB IQA is now strictly `solidos_totais` (Total Solids / Resíduo Total, weight 0.08). TDS (`tds`) is no longer accepted silently as a synonym for Total Solids; providing `tds` in strict CETESB mode throws an explicit, informative error.
- **Stoichiometric phosphorus conversion**: Total phosphorus (mg P/L) is now converted to phosphate via $PO_4 = P_{\text{total}} \times 3.066$ by default (`phosphorus_basis = "P"`), matching the calibration basis of the CETESB sub-index curve.
- **Separation of water temperature and $\Delta T$**: Water temperature (`temperatura`) is used exclusively to compute DO saturation percentage (accounting for altitude via `altitude_m`), while temperature change $\Delta T$ (`delta_temperatura`) is an independent component defaulting to $Q_i = 94$ (operational hypothesis of thermal equilibrium).
- **Coliforms & E. coli**: Canonical parameter is `coliformes_termotolerantes`. E. coli inputs are converted using the CETESB factor $1.25\times$ via `microbial_type = "e_coli"`.
- **Strict calculation by default**: `iqa()` with `method = "CETESB"` requires all 9 components. Partial calculation now requires explicit `allow_partial = TRUE` and flags missing components in metadata.
- **Official CETESB classification scale**: Default breaks for `classify_iqa()` updated to official CETESB categories: Péssima ($\le 19$), Ruim ($19\text{--}36$), Regular ($36\text{--}51$), Boa ($51\text{--}79$), Ótima ($79\text{--}100$).

## New features

- `iqa_components()` and `iqa(..., details = TRUE)`: Comprehensive audit layer returning raw values, transformed values, $Q_i$, $W_i$, and $Q_i^{W_i}$ for each parameter.
- Explicit censored data policy (`censor_policy = c("limit", "half_limit", "preserve", "zero", "na")`).
- Extensive scientific test suite and benchmark against official INEMA SEIA Campanha 3/2024 (Rio Buranhém) monitoring records.

# tikatuwq 0.9.0

## Breaking changes

- `iqa()` now defaults to the correct **weighted geometric mean** (`method = "CETESB"`)
  as specified by CETESB and the original NSF WQI (Brown et al., 1970).
  The previous arithmetic mean behaviour is preserved via `method = "NSF_approx"`.
  Existing code using `iqa()` without specifying `method` will produce different
  (more accurate) IQA values. (#P0)

## New functions

- `mk_seasonal()`: seasonal Mann-Kendall trend test (Hirsch, Slack & Smith,
  1982). Splits the time series by season (monthly or wet/dry via
  `assign_season()`), computes S and VAR(S) per season, aggregates, and
  returns Z, p-value, Kendall tau, and Sen's slope (units/year) per group.
  Removes the seasonal bias that inflates false positives in standard
  Mann-Kendall tests applied to tropical rivers. (#P6)
- `plot_map_quality()`: interactive Leaflet map coloring sampling sites by
  IQA, IET (Carlson or Lamparelli), or NSF WQI value. Uses the standard
  classification palettes for each index and shows class and value in the
  popup. Extends `plot_map()` with index-aware choropleth logic. (#P12)
- `balnear_check()`: classifies sampling sites for primary-contact
  recreation (swimming) under CONAMA Resolution 274/2000 (Excelente,
  Muito Boa, Satisfatória, Imprópria) based on the 80% rule over the last
  5 collections. Supports thermotolerant coliforms and E. coli; flags
  sites with insufficient samples. (#P16)
- `conama_freq_check()`: implements the legal frequency rule of CONAMA 357/2005
  Art. 15 — conformity assessed as ≥80% of samples within limits, requiring
  at least 6 samples per year. Returns a tibble with per-point, per-year,
  per-parameter compliance status. (#P1)
- `assign_season()`: adds a hydrological season column (`"chuvoso"` / `"seco"`)
  to a data frame, based on regional Brazilian calendars (Sudeste, Nordeste,
  Norte, Sul, Centro-Oeste, Bahia) or a custom month vector. (#P3)
- `compare_seasons()`: compares a water quality parameter between wet and dry
  seasons using Wilcoxon, t-test, or Kruskal-Wallis; returns descriptive
  statistics, test results, and optionally a `ggplot` boxplot. (#P3)
- `plot_iet()`: bar chart (vertical or horizontal) for the Trophic State Index
  with color-coded trophic classes, supporting both Carlson (1977) and
  Lamparelli (2004) classification schemes. (#P4)
- `compute_load()`: computes pollutant load as concentration × flow × unit
  factor; supports kg/day, t/day, kg/year, and g/s outputs. (#P7)
- `exceedance_prob()`: estimates empirical exceedance probability with Wilson
  confidence interval, by group. (#P8)
- `wq_pca()`: PCA wrapper around `stats::prcomp()` with automatic column
  selection, biplot, screeplot, and loadings plot as `ggplot` attributes. (#P15)
- `nsfwqi()`: updated to use weighted geometric mean (consistent with CETESB
  IQA fix), adds `add_status` and `locale` arguments, removes prototype
  language. (#P5)

## Data

- `inst/extdata/conama_limits.csv` expanded from ~38 to ~116 rows, adding
  nitrogen species (NO3, NO2, NH3 with pH-conditional limits), inorganic ions
  (fluorides, chlorides, sulfates), organic pollutants (phenols, surfactants),
  and 14 heavy metals / trace elements for Classes 1–3. (#P2)

## Internal

- `R/testando.R`: replaced empty file with documented placeholder comment.
- `R/wq_buranhem.R`: removed duplicate `@docType` tag that caused a `R CMD check`
  WARNING.
- `DESCRIPTION`: version bumped to 0.9.0; `tools` added to Imports.
- Tests added for all new modules (`test-iqa-geometric.R`, `test-conama_freq.R`,
  `test-seasonal.R`, `test-load_exceedance.R`, `test-plot_iet.R`,
  `test-wq_pca.R`).

# tikatuwq 0.8.2

- **CRAN maintenance**: Patch release for CRAN compliance. Fixed example in `plot_map()` to use internal dataset `wq_demo` instead of external file reference. All examples and tests now comply with CRAN policies (no local file dependencies, write only to tempdir). No API changes.

# tikatuwq 0.8.1

- **Fix (CRAN compliance)**: `render_report()` now copies the template to a temporary directory before rendering to comply with CRAN filesystem policies. All intermediate files generated during rendering are written only to `output_dir` (default `tempdir()`), never to the package installation directory. Tests for `render_report()` are now skipped on CRAN (`skip_on_cran()`) and use `withr::local_tempdir()` for proper cleanup.

# tikatuwq 0.8.0

- **Data and documentation update**: The `wq_demo` example dataset is now a subset of real data (from BURANHEM river monitoring, 4 points, years 2020–2024, 20 rows). Includes extra columns: `rio`, `lat`, and `lon`.
- All examples and vignettes now use real, representative dataset (greater realism and reproducibility).
- Documentation (`R/data_wq_demo.R`, inst/extdata/wq_demo_README.txt, vignettes, README) updated to reflect the change.
- No API breakage; all functions and test expectations remain valid for the new dataset structure and size.

# tikatuwq 0.7.3

- Funções `iet_carlson()` e `iet_lamparelli()` agora aceitam `data.frame` com `.keep_ids`.
- Conversão automática de formato BR ("," / "<" / ">") e de `p_total` (mg/L) → `tp` (µg/L).
- `iqa()` mais robusto: alias `temp` para `temperatura`, numificação segura e reponderação com `na_rm=TRUE`.
- Campos identificadores (rio, ponto, data, lat, lon) agora são automaticamente reconhecidos e preservados via `.keep_ids`.

# tikatuwq 0.7.2

Changes in this version:
- Fixed incoming NOTE detected in pre-tests:
  - Removed non-standard fields (`DOI`, `Citation`) from DESCRIPTION.
  - Updated `inst/CITATION` to use the modern `bibentry()` format instead of the deprecated `citEntry()`.
- Minor internal documentation updates (no API or functional changes).

# tikatuwq 0.7.1

- **Title** field adjusted per CRAN request: now *"Water Quality Assessment and Environmental Compliance in Brazil"*.
- Added **Zenodo DOI** and formal citation entry (`citation("tikatuwq")`).
- Updated **README** (EN/PT) with citation, DOI badge, and institutional context.
- No changes in functions or internal code.

# tikatuwq 0.7.0

- New functions `param_analysis()` and `param_analysis_multi()`:
  - Enable detailed analysis of specific parameters by point, river, or group of points.
  - Support cross-comparisons (multiple parameters at one point or multiple points for the same parameter).
  - Include simple temporal trend detection and basic statistical summary.
  - Return standardized data frames compatible with visualization and reporting tools.
- Automated tests for new modules (`test-param_analysis.R`, `test-param_analysis_multi.R`).
- Code updated to meet CRAN best practices (all checks passed 100% cleanly).
- Minor stability improvements in `plot_trend()` and `plot_map()` (controlled messages).

# tikatuwq 0.6.2

- Fix: resolved codoc WARNING for `generate_analysis()` (docs matched to code).
- No functional code changes.

# tikatuwq 0.6.1

- Maintenance update requested by CRAN.
- Fixed relative link `README-pt.md`, now using absolute HTTPS URL.
- No functional changes in code.

# tikatuwq 0.6.0

- New function `plot_trend()`:
  - Visualize time series for parameters (e.g., turbidity, OD, IQA).
  - Add trend lines by group/point using three methods:
    - **Theil–Sen** (robust to outliers),
    - **OLS** (ordinary least squares),
    - **LOESS** (smoothed curve).
  - Supports faceting by river/point and customizable number of samples.
  - Returns a `ggplot` object ready for plotting.
- Updated documentation for `plot_trend()` with practical examples.
- Updated pkgdown site with dedicated section for trend visualization.

# tikatuwq 0.5.1

- Fix: replaced broken URLs in help pages.

# tikatuwq 0.5.0

- New: `plot_map()` — interactive map of sampling points using **Leaflet** (optional dependency via Suggests).
- Examples wrapped in `if (interactive())` / `\donttest{}` to avoid network calls on CRAN.
- Tests: added `skip_on_cran()` and `skip_if_not_installed("leaflet")` guards for map tests.
- Encoding hardening: replaced non-ASCII characters with `\u` escapes.
- Dependencies: removed unused `htmltools` from Imports.
- Docs: roxygen updates; added function to “Visualizations” reference.
- No breaking changes; existing public API unchanged.

# tikatuwq 0.4.6

- DESCRIPTION rewritten in English-only to avoid CRAN “Possibly misspelled words” NOTE.
- Fixed DESCRIPTION URLs (Lamparelli 2004, CONAMA 357/2005).
- Updated spell-check list (`inst/WORDLIST`).
- Added expanded acronyms and DOIs/URLs in DESCRIPTION.
- Documented `@return` for all exported functions.
- Maintained MIT license text (per CRAN policy).
- Minor internal refactor: `conama_limits()` now accepts `class` argument for filtering.

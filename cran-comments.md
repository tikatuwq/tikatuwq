## Submission of tikatuwq 0.10.0

This is a minor release (0.10.0) implementing scientific and metrological updates to the Brazilian Water Quality Index (IQA CETESB/INEMA) formulations:

* **Official Analytical Equations**: Replaced legacy curve interpolations with the official validated piecewise analytical equations for all 9 CETESB sub-indices (DO saturation corrected for temperature and altitude, exact exponential-base curves for BOD5, Total Nitrogen, Turbidity, and Total Phosphorus).
* **Phosphorus Conversion**: Added automatic stoichiometric conversion from Total Phosphorus (P) to phosphate (PO4) with the official 3.066x factor.
* **Microbial Indicator**: Native support for E. coli applying the CETESB 1.25x factor.
* **Component-Level Audit**: Added `iqa_components()` returning raw values, analytical sub-indices (Qi), official weights (Wi), and weighted terms for complete transparency.
* **Solids vs. TDS**: Strict separation of Total Solids (solidos_totais/residuo_total) vs. Total Dissolved Solids (TDS) in strict mode.
* **CONAMA 357/2005**: Added aquatic environment context support (lotic, lentic, intermediate) for total phosphorus standards and pH-conditioned ammonia limits in `conama_check()`.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test results

299 tests pass locally with testthat. Validated against official INEMA monitoring data from Rio Buranhem (2024 Campaign 3).

## Test environments
- Windows 11, R 4.5.1 (x86_64, ucrt)
- macOS 14 (local), R 4.3+
- Ubuntu 22.04 (CI), R 4.3+

## R CMD check results
0 errors | 0 warnings | 0 notes

## Changes
- v0.7.3 (patch, backward-compatible):
  * iqa(): alias `temp` para `temperatura`, saneamento numérico (vírgula decimal, < >), reponderação com `na_rm=TRUE`.
  * iet_carlson() / iet_lamparelli(): aceitam `data.frame`, auto-detecção de `secchi/chla(clorofila)/tp(p_total)`, conversão `p_total (mg/L) → tp (µg/L)`, `.keep_ids` para preservar identificadores.
  * Sem novas dependências; API retrocompatível.

## Reverse dependencies
- No reverse dependencies at this time.

# API simplificada de `iqa()` — tikatuwq 0.11.0

- **Data:** 2026-10-15
- **Base:** `dcbfe7e`

## 1. Assinaturas

| Versão | `iqa()` |
|---|---|
| 0.10.0 (CRAN) | `iqa(df, method = c("CETESB", "CETESB_equations", "CETESB_legacy_approx", "NSF_approx"), classification = c("cetesb", "legacy"), altitude_m, ..., pesos, ...)` |
| `dcbfe7e` (desenvolvimento) | `classification = NULL` depreciado, com aviso de "classificação cruzada"; `scheme` aceito via `...` |
| **0.11.0 (agora)** | `iqa(df, method = c("CETESB", "CETESB_equations", "IGAM", "CETESB_legacy_approx", "NSF_approx"), altitude_m = 0, phosphorus_basis, microbial_type, temperature_method, temperature_reference, delta_temperature, censor_policy, allow_partial, details, add_status, locale, pesos, ...)`. **Sem `classification`** |

`classify_iqa(x, classification = c("cetesb", "igam", "ana", "legacy"), locale, scheme = NULL)`
fica **inalterada**. `scheme` continua depreciado, com aviso.

## 2. Mudança incompatível (atenção)

A especificação assumiu que `classification` em `iqa()` só existia no ramo de
desenvolvimento. **Não é o caso:** a 0.10.0 publicada no CRAN aceitava
`iqa(df, classification = "legacy")`. A remoção foi feita como pedido, mas
documentada como incompatível:

- `iqa(df, classification = ...)` e `iqa(df, scheme = ...)` geram **erro**
  com a orientação "use `classify_iqa(x, classification = ...)`". Não é
  aceitação silenciosa.
- Migração: `classify_iqa(iqa(df)$IQA, classification = "legacy")`.
- Chamadas **posicionais** da 0.10.0 com a classificação como 3º argumento
  (`iqa(df, "CETESB", "legacy")`) passam a atribuir `"legacy"` a
  `altitude_m` e falham na validação ("finite numeric"). Uso raro, mas a NEWS
  registra.
- Uso interno no pacote, nos exemplos, nas vinhetas e nos READMEs: nenhum
  (verificado por `grep`); só os testes usavam e foram reescritos.

## 3. Código e atributos removidos

- Resolução de `classification`/`scheme` e o caminho de "classificação
  cruzada" (avisos e marcação `"cross_classification"` em `iqa_nonstandard`).
- `attr(, "iqa_classification")` permanece e agora é sempre a escala do método
  (`"cetesb"`; `"igam"` quando o motor for liberado).

## 4. `iqa_methods()`

Colunas novas: `implemented`, `documented`, `reference_validated`,
`available`, `limitation` (substituem `status`). Valores:

| method | implemented | documented | reference_validated | available |
|---|---|---|---|---|
| CETESB | TRUE | TRUE | TRUE (exemplo publicado; limitações na coluna `limitation`) | TRUE |
| IGAM | FALSE | TRUE | FALSE | FALSE |
| CETESB_legacy_approx | TRUE | TRUE | FALSE | TRUE |
| NSF_approx | TRUE | TRUE | FALSE | TRUE |

## 5. Impacto nos testes e na documentação

- **Testes:** `test-iqa-audit-validation.R` (2 testes reescritos) e
  `test-iqa-igam-compare.R` (1 teste reescrito, 2 ampliados). Totais: 35
  arquivos, 160 testes, 537 verificações, 0 falhas.
- **Documentação:** `?iqa` (seção "Metodo x classificacao" e `@param ...`),
  `?iqa_methods`, NEWS e READMEs.
- **Regressão CETESB:** 131 amostras idênticas (valor exibido, valor bruto e
  classe; diferença 0). Ver `REGRESSAO_CETESB_POS_API.csv`.

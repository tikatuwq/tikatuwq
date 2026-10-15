# Validação do método CETESB — tikatuwq 0.11.0

- **Data:** 2026-10-15 (3ª rodada)
- **Base:** commit `324ff54` (linha de base: 33 arquivos de teste, 146 testes, 455 verificações aprovadas, 0 falhas)
- **Fronteiras:** `FRONTEIRAS_QI.csv`, com 34 fronteiras e ε = 1e-6
- **Trechos:** `MATRIZ_VALIDACAO_QI.csv`
- **DBO e NT em detalhe:** `VALIDACAO_DBO_NITROGENIO.md`

Status de evidência:
- **(a)** texto ou nota oficial (CETESB 2023, Apêndice D);
- **(b)** equação de fonte secundária rastreável: CPRH PNMA II e von Sperling (2007) via IBEAS (2015);
- **(c)** derivação própria;
- **(d)** sem fonte.

Classificação de saltos: **a** erro de código ou transcrição; **b** comportamento da referência; **c** indeterminado.

## 1. Referência por equação

| Componente | Equação no código (`R/iqa_equations.R`) | Referência | Status |
|---|---|---|---|
| Agregação | IQA = ∏ qi^wi | CETESB 2023, p. 3 | (a) |
| Pesos | 0,17 / 0,15 / 0,12 / 0,10 ×4 / 0,08 ×2 | CETESB 2023, Figura 1 | (a) |
| OD: saturação | Cs = (14,62 − 0,3898T + 0,006969T² − 0,00005896T³)(1 − 2,28675e-5 h)^5,167 | CPRH; von Sperling/IBEAS | (b) |
| OD: qi (4 trechos) | polinômios / potência por %sat (50, 85, 100, 140) | CPRH; von Sperling/IBEAS; gabarito IBEAS ±0,15 | (b) |
| OD > 140 % | 47 | CETESB 2023, nota da Figura 1 (as secundárias dizem 50) | (a) |
| Coliformes (3 trechos) | 100; 100 − 33·L; 100 − 37,2·L + 3,60743·L², com L = log10 C | CPRH; von Sperling/IBEAS; gabarito IBEAS | (b) |
| Coliformes > 10⁵ | 3 | nota da Figura 1 | (a) |
| E. coli | × 1,25 | CETESB 2023, p. 5 | (a) |
| pH < 2 e > 12 | 2 e 3 | nota da Figura 1 | (a) |
| pH 2–8,5 (5 trechos) | quadráticas e reta | CPRH; von Sperling/IBEAS; gabarito IBEAS | (b) |
| pH 8,5–9 | 1415823·exp(−1,1507·pH) | CPRH; IBEAS (EXP); curva q2 | (b) |
| **pH 9–10** | **288 − 27·pH** | CPRH "228 − 27pH" (erro tipográfico) × IBEAS "50 − 32(pH − 9)" | **(b) em conflito; decisão do mantenedor** |
| pH 10–12 | 633 − 106,5·pH + 4,5·pH² | CPRH; von Sperling/IBEAS | (b) |
| DBO (4 trechos) | ver `VALIDACAO_DBO_NITROGENIO.md` | — | (b), com 5–15 em conflito, resolvido pela curva |
| NT (4 trechos) | idem | — | idem |
| PT (3 trechos) | sobre PO₄ | CPRH; von Sperling/IBEAS; gabarito IBEAS | (b) |
| P → PO₄ | × 3,066 | CETESB 2023, p. 5 | (a) |
| PO₄ > 10 | 5 | nota da Figura 1 | (a) |
| Turbidez (2 trechos) | quadrática; exponencial | CPRH; von Sperling/IBEAS; gabarito IBEAS | (b) |
| Turbidez > 100 | 5 | nota da Figura 1 | (a) |
| Sólidos totais (2 trechos) | quadrática; reta | CPRH; von Sperling/IBEAS; gabarito IBEAS | (b) |
| ST > 500 | 32 | nota da Figura 1 | (a) |
| Temperatura `cetesb_default` | qi = 94 | CPRH; IBEAS ("constante de 94 pela CETESB"); compatível com q^w = 1,58 (p. 5) | (b) |
| Temperatura `delta`/`reference` | 3 trechos sem fonte | nenhuma | **(d): experimental, com aviso** |
| ΔT < −5 | NA | nota da Figura 1 ("indefinido") | (a) |
| ΔT > 15 | 9 | nota da Figura 1 | (a) |
| Dados ausentes | IQA inviabilizado | CETESB 2023, p. 4 (`allow_partial = FALSE`) | (a) |
| Classes | 19/36/51/79, fechados à direita, sobre o valor não arredondado | Tabela 1, p. 5 | (a) |

## 2. Fronteiras prioritárias (de `FRONTEIRAS_QI.csv`)

| Parâmetro | Limite | qi(x − ε) | qi(x) | qi(x + ε) | Salto | Classe | Conclusão |
|---|---:|---:|---:|---:|---:|:---:|---|
| DBO | 5 | 53,969 | 53,969 | 53,898 | −0,071 | b | Corrigido na 0.11.0 (v0.10.0: +28,65) |
| DBO | 15 | 19,241 | 19,241 | 19,634 | +0,393 | b | Artefato do ajuste publicado |
| DBO | 30 | 4,915 | 4,915 | 2,000 | −2,915 | b | Valor fixo oficial |
| NT | 10 | 48,900 | 48,900 | 48,705 | −0,195 | b | Corrigido (v0.10.0: +29,9) |
| NT | 60 | 7,311 | 7,311 | 7,344 | +0,032 | b | Artefato do ajuste |
| NT | 100 | 0,944 | 0,944 | 1,000 | +0,056 | b | Ajuste × valor fixo oficial |
| pH | 8,5 | 80,000 | 80,000 | 80,020 | +0,020 | b | Corrigido (v0.10.0: −80) |
| pH | 9 | 45,012 | 45,012 | 45,000 | −0,012 | **c** | Fontes secundárias divergem |
| pH | 10 | 18,000 | 18,000 | 18,000 | 0 | b | — |
| OD | 50 / 85 / 100 % | — | — | — | ≈ 0 | b | Contínuo |
| OD | 140 % | ≈ 74,3 | ≈ 74,3 | 47 | ≈ −27 | b | Valor fixo oficial |
| ΔT | 5 | 83,5 | 83,5 | 90,5 | +7,0 | **c** | Curva sem fonte; modo experimental |

Não resta nenhum salto do tipo **a** no código atual.

## 3. Demais verificações

- **Implementação independente** (`audit/iqa_independent.R`): 3000 entradas, diferença máxima de 1,4e-14, nenhuma classe diferente.
- **Gabarito externo:** 21 valores de qi do IBEAS e o exemplo de von Sperling (IQA = 81).
- **Altitude:** aceita escalar ou um valor por linha, entre −500 e 9000 m. Testado em `test-iqa-audit-validation.R`.
- **Censura** ("<LD", "<3"): segue `censor_policy`; o padrão `limit` usa o próprio limite. Testado.
- **Pesos customizados:** os detalhes mostram os pesos efetivos, e `prod(qi^wi)` reproduz `IQA_unrounded` (testado com e sem `allow_partial`).
- **Consistência entre `iqa_components()` e `iqa()`:** testada.

## 4. Itens sem validação

1. Os coeficientes por trecho não têm fonte oficial publicada; a CETESB só publica as curvas.
2. pH 9–10: decisão do mantenedor.
3. Curva de ΔT nos modos `delta`/`reference`: sem fonte, mantida como experimental.
4. von Sperling (2007) não foi consultado diretamente; a transcrição usada é a do IBEAS.

# Motor IGAM experimental: metodologia, entradas e unidades

> **Atualização (2026-10-16):** o mantenedor autorizou a liberação. O
> método IGAM agora está disponível em `iqa(df, method = "IGAM")`, com
> `available = TRUE` e `reference_validated = TRUE` (reprodução de IQAs
> publicados, **não** homologação). Ver `IGAM_RELEASE_READINESS.md` e
> `inst/methodology/IGAM_VALIDATION.md`. O texto abaixo registra o estado
> anterior à liberação.

- **Data:** 2026-10-15
- **Código:** `R/iqa_igam.R` (`.iqa_igam_compute()`, interna)
- **Estado:** implementado; **não liberado** em `iqa()`.

**Categorias de status usadas abaixo:**
- **(a)** fonte institucional literal;
- **(b)** secundária;
- **(c)** hipótese documentada na literatura;
- **(p)** política do pacote, sem regra institucional.

## 1. Dicionário de entradas

| Parâmetro | Coluna (aliases) | Unidade exigida | Conversão | Status |
|---|---|---|---|---|
| Oxigênio dissolvido | `od` | mg/L | Para % de saturação: 100·OD/Cs | (a) SCQA p. 3–4 |
| Temperatura da água | `temperatura` | °C | Só para o Cs | (a) SCQA p. 3 |
| Cloreto | `cloreto` (`cloreto_total`, `chloride`) | mg/L Cl | Só para o Cs | (a) SCQA p. 3 |
| Altitude | argumento `altitude_m` (um valor ou um por linha) | m | Só para o Cs | (a) SCQA p. 3 |
| Microbiológico | `e_coli` (preferida) ou `coliformes_termotolerantes` | NMP/100 mL | **Nenhuma** (sem o fator 1,25 da CETESB) | (a) relatório IGAM 2017, p. 9 (*E. coli*). (c) sem fator: nos 15 casos da investigação, o fator 1,25 piora todas as combinações (`igam_variantes.csv`) |
| pH | `ph` | — | — | (a) |
| DBO | `dbo` | mg/L | — | (a) |
| Nitrato | `nitrato` (`n_nitrato`, `no3`, `nitrate`) | Declarada em `nitrate_unit`: `"N"` ou `"NO3"` (**obrigatório**, sem padrão) | `"N"`: × 62,004/14,007 = 4,4266 | (a) curva em mg/L NO₃⁻ (SCQA, Tabela 2.1; portal IGAM, Tabela 1). Dados IGAM em mg/L N (relatório 2017, Anexo I) |
| Fosfato total | `phosphorus_input = "PO4"`: `fosfato_total`; `"P"`: `p_total` | mg/L PO₄, ou mg/L P | `"P"`: × 94,9714/30,9738 = 3,0662 | (a) curva em PO₄. (b/c) a conversão de P é confirmada empiricamente |
| Turbidez | `turbidez` | UNT | — | (a) curva. (c) k = 1,5 |
| Sólidos totais | `solidos_totais` | mg/L | **Nunca** substituídos por TDS | (a) |
| Variação de temperatura | — | — | qs constante = 92 | (a) portal IGAM (Projeto Águas de Minas) |

**Substituições proibidas** (não ocorrem e geram diagnóstico):
- `nt_total` no lugar do nitrato;
- `p_ortofosfato` no lugar do fosfato total;
- `p_total` sem `phosphorus_input = "P"`;
- `tds` no lugar dos sólidos totais.

## 2. Fórmulas (SCQA 2005)

As equações estão transcritas em `SCQA_FONTES_E_EQUACOES.md`. No código:
- `.igam_cs()`;
- `.qi_igam_od()`, com y₃ lido como multiplicação;
- `.qi_igam_coliformes()`, com log₁₀;
- `.qi_igam_ph()`, `.qi_igam_dbo()`;
- `.qi_igam_nitrato()`, com ln no trecho 10–60;
- `.qi_igam_fosfato()`;
- `.qi_igam_turbidez()`, com k = 1,5;
- `.qi_igam_solidos()`.

**Agregação:** IQA = ∏ qᵢ^wᵢ.

**Pesos:**

| OD | Microbiológico | pH | DBO | NO₃ | PO₄ | ΔT | Turbidez | ST |
|---|---|---|---|---|---|---|---|---|
| 0,17 | 0,15 | 0,12 | 0,10 | 0,10 | 0,10 | 0,10 | 0,08 | 0,08 |

- **Truncamento:** os qi não são limitados a 100, porque o SCQA não define
  isso. Os máximos da fonte são OD 100,02, nitrato 100,17 e turbidez 100,003.
- **Descontinuidades da própria fonte:** são mantidas, testadas e não
  suavizadas.

| Parâmetro | Ponto | Comportamento |
|---|---|---|
| OD | 100 % | 99,6 → 100,02 |
| pH | 6,9 e 7,1 | Saltos de cerca de −2,9 e −2,8 |
| pH | 12 | 2,19 → 3 (o texto se sobrepõe; adotado "≤ 12" para o polinômio) |
| Turbidez | 100 UNT | 16,86 → 5 |
| Sólidos totais | 500 mg/L | 33,95 → 30 |

**Classificação:** escala `"igam"` de `classify_iqa()`, vinculada ao motor.
- Muito Ruim: 0–25;
- Ruim: 25–50;
- Médio: 50–70;
- Bom: 70–90;
- Excelente: 90–100.

O limite superior pertence à classe (fonte: SCQA, Tabela 2.2, e relatório IGAM
2017).

**Saída:**
- `IQA_raw`, sem arredondamento;
- `IQA`, com 1 casa, como o IGAM publica;
- `IQA_status`;
- qi e pesos efetivos por parâmetro;
- `observacao`, com o motivo de exclusão ou de redistribuição.

## 3. Ausências

| Situação | Regra | Status |
|---|---|---|
| OD ou microbiológico ausente ou inválido | IQA não calculado (`NA`, com motivo) | (a) portal IGAM |
| Temperatura ou cloreto ausente | Cs não calculável → OD indisponível → IQA não calculado. **Nenhum valor padrão de cloreto** é assumido | (p) o SCQA exige cloreto; o IGAM não documenta o que fazer sem ele |
| Demais parâmetros ausentes | Peso repartido igualmente entre os restantes | (a) portal IGAM. Com mais de um ausente, reparte-se a soma (interpretação) |
| Microbiológico < 1 NMP/100 mL | qi `NA` (fora do domínio do SCQA) → IQA não calculado | (p) o domínio não é definido pela fonte |
| Valor negativo, pH fora de 0–14, temperatura fora de −2 a 50 °C | `NA` com aviso. Sem truncamento silencioso | (p) |

## 4. Censura

- **Argumento:** `censor_policy`, com padrão `"limit"`. As alternativas são
  `"half_limit"`, `"zero"` e `"na"`.
- **Registro:**
  - a política fica em `attr(, "igam_config")$censor_policy`;
  - os valores censurados de cada linha e parâmetro ficam em
    `attr(, "igam_censored")`.
- **Status (p):** o IGAM não documenta a regra. `"limit"` foi a convenção que
  reproduziu 49/49 IQAs publicados, 35 deles com algum dado censurado. Isso é
  evidência empírica, não regra institucional.

## 5. Arredondamento

- O IGAM publica com 1 casa decimal.
- O motor guarda o valor bruto e arredonda só a exibição.
- A regra de arredondamento do IGAM (meio para cima ou para par) não está
  documentada.
- Nos 49 casos, a menor distância de um valor bruto a um ponto de desempate
  (x,x5) foi 0,0011. Por isso a regra não pôde ser testada.

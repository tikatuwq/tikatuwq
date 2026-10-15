# Validação de DBO e nitrogênio total — tikatuwq 0.11.0

- **Data:** 2026-10-15
- **Base:** commit `4e88fa9` (v0.10.0); working tree na branch `fix/iqa-scientific-stabilization`
- **Valores numéricos:** `audit/iqa_boundaries.R` → `fronteiras_qi.csv` (ε = 1e-6)
- **DBO e nitrogênio total permanecem no IQA.** Nenhum parâmetro foi removido.

## 1. Fontes e status de evidência

| Fonte | O que traz | Status |
|---|---|---|
| CETESB (2023), Apêndice D, Figura 1 (p. 4) | Curvas gráficas q3 (DBO) e q4 (NT); notas "DBO > 30 → q3 = 2" e "NT > 100 → q4 = 1"; NT "por métodos diretos ou pela soma de suas formas" | (a) oficial literal para as notas; curva só por leitura visual |
| CPRH, PNMA II — *Índices e indicadores de qualidade da água* (fórmulas da planilha Excel da CETESB) | Equações por trecho; escreve "log" em DBO 5–15 e NT 10–60 (base não especificada) | (b) secundária |
| von Sperling (2007), *Estudos e modelagem da qualidade da água de rios*, p. 255, reproduzido por IBEAS (2015), trabalho VIII-041, Tabela 2 | Mesmas equações, com **LOG10** nos dois trechos | (b) secundária. O livro não foi consultado diretamente |
| IBEAS (2015), Tabelas 8 e 10 | Valores calculados por planilha: DBO 20 → 11,05; **DBO 12 → 70,60** (coerente com log10) | (b) gabarito externo de terceiro |

Os coeficientes são idênticos nas duas transcrições secundárias, inclusive no
trecho 15–30 (`4394,91·C^-1,99809`) e em NT 60–100 (`159,3148·e^(-0,0512842·C)`,
que a CPRH escreve com "2,7828" por erro tipográfico). **A única divergência é
a base do logaritmo.**

## 2. DBO (`.qi_cetesb_dbo`)

### 2.1 Trechos

| Trecho (mg/L) | v0.10.0 | 0.11.0 | Fonte |
|---|---|---|---|
| 0 ≤ C ≤ 5 | `99.96·exp(-0.1232728·C)` | idem | (b) |
| 5 < C ≤ 15 | `104.67 - 31.5463·log10(C)` | `104.67 - 31.5463·ln(C)` | (b), em conflito; escolha pela curva oficial |
| 15 < C ≤ 30 | `4394.91·C^(-1.99809)` | idem | (b) |
| C > 30 | 2 | 2 | (a) nota da Figura 1 |

Entradas: DBO negativa, `Inf` ou caractere não numérico → `NA` com aviso.
Valores censurados ("<3") seguem `censor_policy` (padrão: limite). Não há
substituição automática por COT.

### 2.2 Fronteiras

| x (mg/L) | qi(x − ε) | qi(x) | qi(x + ε) | salto 0.11.0 | salto v0.10.0 | Leitura |
|---:|---:|---:|---:|---:|---:|---|
| 0 | 99,960 | 99,960 | 99,960 | 0 | 0 | — |
| 5 | 53,9688 | 53,9688 | 53,8982 | −0,071 | **+28,65** | A v0.10.0 melhorava a qualidade com mais DBO |
| 15 | 19,2410 | 19,2410 | 19,6342 | **+0,393** | **−47,93** | Ver 2.3 |
| 30 | 4,9151 | 4,9151 | 2,0000 | −2,915 | −2,915 | Regra fixa oficial (DBO > 30 → 2); **não é defeito** |

### 2.3 Transição em 15 mg/L

- Os dois ramos (`104,67 − 31,5463·ln C` e `4394,91·C^-1,99809`) aparecem com
  coeficientes idênticos nas duas transcrições independentes. Não há indício
  de coeficiente errado.
- Nenhuma escolha de base fecha os dois lados. Com `ln`, os ramos diferem 0,39
  em 15 (2%). Com `log10`, a diferença seria de 47,9.
- Conclusão: o salto de +0,39 é um **artefato do ajuste por trechos**, em que
  cada ramo foi ajustado separadamente à curva gráfica. Não se impõe
  continuidade, porque nenhuma fonte a exige e alterar coeficientes seria
  inventá-los.
- Faixa não monotônica: qi(C) > qi(15) para 15 < C < 15,153 mg/L. Efeito
  máximo no IQA: (19,634/19,241)^0,10 = +0,2%.
- Os testes verificam monotonicidade em passos de 0,5 mg/L e os valores dos
  dois lados da fronteira de 15 mg/L (`test-iqa-audit-validation.R`).

### 2.4 Comparação com a curva oficial (leitura visual, ±5 qi)

| DBO (mg/L) | v0.10.0 | 0.11.0 | Figura 1 (visual) |
|---:|---:|---:|---:|
| 2 | 78,1 | 78,1 | ~78 |
| 5 | 54,0 | 54,0 | ~55 |
| 10 | **73,1** | 32,0 | ~30 |
| 12 | **70,6** | 26,3 | ~25 |
| 15 | 67,6 → 19,6 | 19,2 → 19,6 | ~20 |
| 20 | 11,1 | 11,1 | ~12 |
| 30 | 4,9 | 4,9 | ~5 |

### 2.5 Decisão e impacto

- **Decisão:** manter `ln` no trecho 5–15. Status: *implementado como
  aproximação consistente com a curva oficial; transcrição secundária
  conflitante documentada.* Não é "validado oficialmente": a CETESB não
  publica a equação.
- **Impacto (0.10.0 → 0.11.0):** o IQA cai de 3 a 9 pontos para DBO entre 5 e
  15 mg/L, com o resto da amostra fixo. Na grade, DBO 5,001 passa de Ótima
  (79,3) para Boa (76,0); no gradiente de poluição, 2 amostras passam de Boa
  para Regular; no `wq_demo`, 2 amostras caem cerca de 5 pontos, sem mudar de
  classe.
- **Aviso a usuários:** resultados de planilhas que seguem von Sperling com
  `log10` (por exemplo, IBEAS 2015) serão mais altos nessa faixa. A variante
  `log10` daria até +8,5 pontos de IQA (`sensibilidade_variantes.csv`).

## 3. Nitrogênio total (`.qi_cetesb_nt`)

### 3.1 Trechos

| Trecho (mg/L N) | v0.10.0 | 0.11.0 | Fonte |
|---|---|---|---|
| 0 ≤ C ≤ 10 | `100 - 8.169·C + 0.3059·C²` | idem | (b) |
| 10 < C ≤ 60 | `101.9 - 23.1023·log10(C)` | `101.9 - 23.1023·ln(C)` | (b), em conflito; escolha pela curva oficial |
| 60 < C ≤ 100 | `159.3148·exp(-0.0512842·C)` | idem | (b) |
| C > 100 | 1 | 1 | (a) nota da Figura 1 |

**Espécie química:** nitrogênio total em mg/L de N (Figura 1: "N. T. mg/L"; p. 4:
total por método direto ou soma de orgânico, amoniacal, nitrato e nitrito).
O pacote **não** converte nitrato em NT. Um método IGAM, que usa nitrato,
teria de ser um método separado e validado.

### 3.2 Fronteiras

| x (mg/L) | qi(x − ε) | qi(x) | qi(x + ε) | salto 0.11.0 | salto v0.10.0 | Leitura |
|---:|---:|---:|---:|---:|---:|---|
| 10 | 48,9000 | 48,9000 | 48,7050 | −0,195 | **+29,90** | A v0.10.0 melhorava a qualidade com mais NT |
| 60 | 7,3112 | 7,3112 | 7,3436 | **+0,032** | **−53,48** | Ver 3.3 |
| 100 | 0,9441 | 0,9441 | 1,0000 | **+0,056** | +0,056 | Ver 3.3 |

### 3.3 Transições em 60 e 100 mg/L

- **60 mg/L:** é o mesmo caso da DBO em 15. Os coeficientes são idênticos nas
  duas transcrições, e os ramos diferem 0,03 (0,4%). É artefato do ajuste. A
  faixa não monotônica vai de 60 a 60,087 mg/L, com efeito no IQA abaixo de 0,1%.
- **100 mg/L:** o ramo exponencial termina em 0,944, abaixo do valor fixo
  oficial de 1. A faixa não monotônica vai de 98,88 a 100 mg/L, com efeito no
  IQA abaixo de 0,6%. O valor fixo é literal na fonte, e mexer no ramo
  exigiria inventar coeficientes. **Mantido e documentado.**
- Os testes verificam monotonicidade em [0, 100] com passo de 1 mg/L e fixam
  os valores em 100 e 100 + ε como comportamento conhecido.

### 3.4 Comparação com a curva oficial (leitura visual, ±5 qi)

| NT (mg/L) | v0.10.0 | 0.11.0 | Figura 1 (visual) |
|---:|---:|---:|---:|
| 2 | 84,9 | 84,9 | ~85 |
| 10 | 48,9 | 48,9 | ~50 |
| 20 | **71,8** | 32,7 | ~32 |
| 40 | **64,9** | 16,7 | ~17 |
| 60 | **60,8** → 7,3 | 7,3 | ~8 |
| 80 | 2,6 | 2,6 | ~3 |

### 3.5 Decisão e impacto

- **Decisão:** manter `ln` no trecho 10–60, com o mesmo status da DBO.
- **Impacto:** o IQA cai de 4 a 15 pontos para NT entre 10 e 60 mg/L. Não houve
  mudança de classe na grade (a amostra-base fica em Boa). O `wq_demo` não tem
  NT acima de 10. A variante `log10` daria até +14 pontos.

## 4. Itens sem validação

1. **Os coeficientes dos ajustes não têm fonte oficial publicada.** A CETESB
   publica só a curva. As duas fontes secundárias concordam nos coeficientes e
   divergem na base do log.
2. **O livro de von Sperling (2007) não foi consultado diretamente.** A
   transcrição usada é a do IBEAS. Conferir a p. 255 do livro resolveria se o
   `LOG10` é do livro ou do IBEAS.
3. A leitura visual da Figura 1 tem incerteza de cerca de ±5 qi. Ela rejeita a
   versão `log10` (erros de 40 a 50 qi), mas não valida coeficientes.

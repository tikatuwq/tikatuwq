# Exemplos Práticos — pacote `tikatuwq`

Todos os exemplos abaixo foram executados e verificados com `tikatuwq` v0.9.0.  
Dataset usado: `wq_demo` — 20 amostras reais do monitoramento INEMA (Bahia, 2020–2024),  
4 pontos de coleta, 5 amostras cada.

---

## 0. Carregar o pacote e o dataset de demonstração

```r
library(tikatuwq)

data("wq_demo", package = "tikatuwq")
dim(wq_demo)
#> [1] 20 14

names(wq_demo)
#> [1] "rio"         "ponto"       "data"        "ph"          "od"
#> [6] "turbidez"    "dbo"         "coliformes"  "p_total"     "nt_total"
#> [11] "temperatura" "tds"         "lat"         "lon"
```

---

## 1. Índice de Qualidade da Água — IQA/WQI (`iqa`)

Calcula o IQA pela **média geométrica ponderada** (metodologia CETESB).

```r
iqa(wq_demo[1:3, ], method = "CETESB")[, c("ponto", "data", "IQA", "IQA_status")]
```

```
# A tibble: 3 × 4
  ponto       data       IQA IQA_status
  <chr>       <date>   <dbl> <chr>
1 FBS-BRH-250 2020-03-03  78.3 Boa
2 FBS-BRH-250 2022-03-10  81.6 Boa
3 FBS-BRH-250 2023-05-24  80.8 Boa
```

**Classificação IQA (CETESB):** Péssima (0–19) · Ruim (20–36) · Regular (37–51) · Boa (52–79) · Ótima (80–100)

---

## 2. Conformidade CONAMA 357/2005 — amostra a amostra (`conama_check`)

Verifica cada amostra individualmente contra os limites da classe.

```r
conama_check(wq_demo[1:2, ], classe = "2")[,
  c("ponto", "data", "ph_ok", "od_ok", "turbidez_ok", "dbo_ok")]
```

```
# A tibble: 2 × 6
  ponto       data       ph_ok od_ok turbidez_ok dbo_ok
  <chr>       <date>     <lgl> <lgl> <lgl>       <lgl>
1 FBS-BRH-250 2020-03-03  TRUE  TRUE  TRUE        TRUE
2 FBS-BRH-250 2022-03-10  TRUE  TRUE  TRUE        TRUE
```

> Retorna uma coluna `_ok` para cada parâmetro com limite definido na classe.  
> `TRUE` = dentro do limite; `FALSE` = fora do limite; `NA` = valor ausente.

---

## 3. Conformidade por frequência — regra dos 80% (`conama_freq_check`)

A Resolução CONAMA 357/2005, Art. 15, exige conformidade em **≥ 80% das amostras**
quando há 6 ou mais amostras por ano. O argumento `min_n` controla esse limiar.

```r
conama_freq_check(wq_demo, classe = "2", by = "ponto", min_n = 1)[1:6,
  c("ponto", "ano", "parametro", "n", "pct_ok", "freq_conforme")]
```

```
# A tibble: 6 × 6
  ponto       ano  parametro     n pct_ok freq_conforme
  <chr>       <int> <chr>     <int>  <dbl> <lgl>
1 FBS-BRH-250 2020  ph            1    100  TRUE
2 FBS-BRH-300 2020  ph            1    100  TRUE
3 FBS-BRH-450 2020  ph            1    100  TRUE
4 FBS-BRH-250 2022  ph            1    100  TRUE
5 FBS-BRH-300 2022  ph            1      0  FALSE
6 FBS-BRH-450 2022  ph            1    100  TRUE
```

> Com `min_n = 6` (padrão legal), `freq_conforme` retorna `NA` quando há menos de 6 amostras
> no grupo (indicando que a regra estatística não se aplica).

---

## 4. Índice de Estado Trófico — Carlson (`iet_carlson`)

Calcula o IET de Carlson (1977) a partir do fósforo total (`p_total`, em mg/L).

```r
iet_carlson(wq_demo[1:3, ])[, c("TSI_TP", "IET", "TSI_status")]
```

```
     TSI_TP      IET      TSI_status
1  22.91085 22.91085 Ultraoligotrofico
2  27.25171 27.25171 Ultraoligotrofico
3  22.91085 22.91085 Ultraoligotrofico
```

**Classificação IET (Carlson/Lamparelli):** Ultraoligotrófico (< 47) · Oligotrófico (47–52) · Mesotrófico (52–59) · Eutrófico (59–63) · Supereutrófico (63–67) · Hipereutrófico (≥ 67)

---

## 5. Índice de Estado Trófico — Lamparelli (`iet_lamparelli`)

Adaptação brasileira de Lamparelli (2004), com equações calibradas para rios tropicais.

```r
iet_lamparelli(wq_demo[1:3, ])[, c("IET_TP", "IET_Lamp", "TSI_status")]
```

```
   IET_TP IET_Lamp    TSI_status
1 23.0103  23.0103 Ultraoligotrofico
2 26.0206  26.0206 Ultraoligotrofico
3 23.0103  23.0103 Ultraoligotrofico
```

---

## 6. NSF Water Quality Index (`nsfwqi`)

Implementa o WQI da National Sanitation Foundation (Brown et al., 1970)
via média geométrica ponderada de 9 parâmetros.

```r
nsfwqi(wq_demo[1:3, ])[, c("ponto", "data", "NSFWQI", "NSFWQI_status")]
```

```
# A tibble: 3 × 4
  ponto       data       NSFWQI NSFWQI_status
  <chr>       <date>      <dbl> <chr>
1 FBS-BRH-250 2020-03-03   80.9 Boa
2 FBS-BRH-250 2022-03-10   84.4 Boa
3 FBS-BRH-250 2023-05-24   81.8 Boa
```

**Classificação NSF WQI:** Muito Ruim (0–25) · Ruim (25–50) · Regular (50–70) · Boa (70–90) · Excelente (90–100)

---

## 7. Balneabilidade CONAMA 274/2000 (`balnear_check`)

Classifica pontos de banho com base na frequência de coliformes termotolerantes.

```r
balnear_check(wq_demo)[, c("ponto", "classificacao", "propria", "n_amostras")]
```

```
# A tibble: 4 × 4
  ponto       classificacao propria n_amostras
  <chr>       <chr>         <lgl>       <int>
1 FBS-BRH-250 Excelente      TRUE            5
2 FBS-BRH-300 Excelente      TRUE            5
3 FBS-BRH-450 Excelente      TRUE            5
4 FBS-BRH-950 Excelente      TRUE            5
```

**Classificação:** Excelente (≤ 200 CF/100 mL em 80% das amostras) · Muito Boa (≤ 500) · Satisfatória (≤ 1000) · Imprópria (> 1000 ou < 80%)

---

## 8. Probabilidade de excedência (`exceedance_prob`)

Estima a probabilidade empírica de um parâmetro ultrapassar um limiar de referência.

```r
exceedance_prob(wq_demo, param = "turbidez", threshold = 40, by = "ponto")
```

```
# A tibble: 4 × 8
  ponto       threshold direction     n n_excedeu prob_excedencia ic_inf ic_sup
  <chr>           <dbl> <chr>     <int>     <int>           <dbl>  <dbl>  <dbl>
1 FBS-BRH-250        40 above         5         2             0.4  0.118  0.769
2 FBS-BRH-300        40 above         5         2             0.4  0.118  0.769
3 FBS-BRH-450        40 above         5         0             0.0  0.000  0.434
4 FBS-BRH-950        40 above         5         0             0.0  0.000  0.434
```

> `ic_inf`/`ic_sup`: intervalo de confiança de Wilson (95%) para a proporção.  
> Para parâmetros com limite *mínimo* (ex.: OD), use `direction = "below"`.

---

## 9. Classificação de período hidrológico (`assign_season`)

Adiciona a coluna `season` ao data frame com base no calendário regional.
**O argumento `region` é opcional** — sem ele, a coluna `season` fica `NA`
(sem erro), e o restante da análise continua normalmente.

### 9.1. Com região definida

```r
d <- assign_season(wq_demo, region = "bahia")
table(d$season)
#> chuvoso    seco
#>       9      11

d[1:4, c("ponto", "data", "season")]
```

```
# A tibble: 4 × 3
  ponto       data       season
  <chr>       <date>     <chr>
1 FBS-BRH-250 2020-03-03 chuvoso
2 FBS-BRH-250 2022-03-10 chuvoso
3 FBS-BRH-250 2023-05-24 seco
4 FBS-BRH-250 2024-02-20 chuvoso
```

**Regiões disponíveis:** `"sudeste"`, `"bahia"`, `"centro_oeste"`, `"nordeste"`, `"norte"`, `"sul"`, `"custom"`.  
Para `"bahia"`, o período chuvoso é outubro–março (Out–Mar).

### 9.2. Sem região (`region = NULL`, padrão)

Se a sazonalidade regional não for relevante para a análise, basta não informar `region`:

```r
d2 <- assign_season(wq_demo)
table(d2$season, useNA = "always")
```

```
assign_season(): nenhuma 'region' ou 'wet_months' foi informada; a coluna
'season' foi preenchida com NA. Para classificar por periodo hidrologico,
informe region (ex.: 'bahia') ou wet_months.

<NA>
  20
```

> Nesse caso, `season` fica `NA` para todas as linhas e uma mensagem informativa
> é exibida — **sem erro**. Funções que não dependem de `season`
> (ex.: `mk_seasonal(period = "monthly")`, o padrão) continuam funcionando normalmente.

### 9.3. Calendário personalizado (`wet_months`)

Para definir os meses chuvosos da sua bacia sem usar uma região predefinida:

```r
d3 <- assign_season(wq_demo, wet_months = c(11, 12, 1, 2, 3))
table(d3$season)
```

---

## 10. Teste de Mann-Kendall sazonal (`mk_seasonal`)

Detecta tendências de longo prazo em séries de qualidade da água,
controlando a sazonalidade (Hirsch, Slack & Smith, 1982).

```r
d <- assign_season(wq_demo, region = "bahia")
mk_seasonal(d, param = "od", by = "ponto", period = "season")
```

```
# A tibble: 4 × 11
  ponto       parametro n_obs n_estacoes  S  varS  Z p_value   tau sen_slope significativo tendencia
  <chr>       <chr>     <int>      <int> <int> <dbl> <dbl> <dbl> <dbl>     <dbl> <lgl>         <chr>
1 FBS-BRH-250 od            5          1    -1  3.67    NA    NA  -0.1        NA  NA            sem_tendencia
2 FBS-BRH-300 od            5          1     1  3.67    NA    NA   0.1        NA  NA            sem_tendencia
3 FBS-BRH-450 od            5          1    -1  3.67    NA    NA  -0.1        NA  NA            sem_tendencia
4 FBS-BRH-950 od            5          1    -2  8.67    NA    NA  -0.2        NA  NA            sem_tendencia
```

> **Nota:** `p_value = NA` porque o dataset de demonstração tem apenas 5 amostras por ponto
> — o teste requer ≥ 3 observações por estação. Em séries reais de monitoramento
> (tipicamente ≥ 10 amostras/ano), `p_value` e `sen_slope` serão calculados normalmente.

**Colunas de saída:**
- `S` / `varS` / `Z`: estatística de Mann-Kendall e sua variância/z-score  
- `tau`: coeficiente de correlação de Kendall (−1 a +1)  
- `sen_slope`: inclinação de Sen em unidade/ano  
- `significativo`: `TRUE` se `p_value < alpha` (padrão 0.05)  
- `tendencia`: `"crescente"`, `"decrescente"` ou `"sem_tendencia"`

---

## Resumo das funções principais

| Função | O que faz | Retorna |
|---|---|---|
| `iqa()` | IQA/WQI pela média geométrica ponderada (CETESB) | df + colunas `IQA`, `IQA_status` |
| `conama_check()` | Conformidade CONAMA 357 amostra a amostra | df + colunas `{param}_ok` |
| `conama_freq_check()` | Regra dos 80% (Art. 15 CONAMA 357) por ponto/ano | tibble longo por parâmetro |
| `iet_carlson()` | IET de Carlson (1977) | df + colunas `TSI_TP`, `IET`, `TSI_status` |
| `iet_lamparelli()` | IET adaptado por Lamparelli (2004) | df + colunas `IET_TP`, `IET_Lamp`, `TSI_status` |
| `nsfwqi()` | NSF WQI (Brown et al., 1970) | df + colunas `NSFWQI`, `NSFWQI_status` |
| `balnear_check()` | Balneabilidade CONAMA 274/2000 | tibble por ponto |
| `exceedance_prob()` | Probabilidade empírica de excedência (Wilson IC) | tibble por grupo |
| `assign_season()` | Classificação hidrológica chuvoso/seco (opcional, `region = NULL` por padrão) | df + coluna `season` |
| `mk_seasonal()` | Teste de Mann-Kendall sazonal (Hirsch et al., 1982) | tibble por grupo |
| `compute_load()` | Carga poluidora (concentração × vazão) | df + coluna de carga |
| `normalize_param_names()` | Normaliza nomes de colunas para nomes canônicos | df renomeado |
| `wq_pca()` | PCA de parâmetros de qualidade da água | lista com scores, loadings e gráfico |
| `read_wq()` | Importa e limpa planilhas de monitoramento (CSV/Excel) | tibble limpo |

---

*Gerado com `tikatuwq` v0.9.0 · Dataset: `wq_demo` (INEMA, Bahia, 2020–2024)*

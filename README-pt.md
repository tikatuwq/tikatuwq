# 💧 TikatuWQ: Um Pacote R para Avaliação da Qualidade da Água e Conformidade Ambiental no Brasil

**Fluxos de trabalho reprodutíveis para monitoramento de águas doces no Brasil, incluindo índices de qualidade da água, verificações regulatórias e geração automatizada de relatórios.**

**TikatuWQ** é um pacote R de código aberto desenvolvido para analisar, visualizar e relatar dados de qualidade da água de acordo com os padrões ambientais brasileiros.  
Implementa os principais índices utilizados no país: **IQA/NSFWQI** e **IET (Carlson e Lamparelli)** e realiza verificações automáticas de conformidade com a **Resolução CONAMA 357/2005** — incluindo a regra de frequência legal (Art. 15).  
O pacote também inclui análise sazonal, cálculo de carga poluidora, probabilidade de excedência, PCA multivariado, análise de tendências, validação de dados e geração automática de relatórios.

[![CRAN status](https://www.r-pkg.org/badges/version/tikatuwq)](https://cran.r-project.org/package=tikatuwq) 
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/tikatuwq)](https://cran.r-project.org/package=tikatuwq) 
[![CRAN monthly downloads](https://cranlogs.r-pkg.org/badges/tikatuwq)](https://cran.r-project.org/package=tikatuwq) 
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17407297.svg)](https://doi.org/10.5281/zenodo.17407297) 
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) 
[![R-CMD-check](https://github.com/tikatuwq/tikatuwq/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tikatuwq/tikatuwq/actions) 
![Status do manuscrito](https://img.shields.io/badge/manuscrito-em_avaliação-yellow) 

---

## Escopo Científico 

O pacote **tikatuwq** foi desenvolvido para apoiar fluxos de trabalho científicos reprodutíveis em monitoramento de águas doces e avaliação ambiental. Bases de dados de monitoramento ambiental frequentemente apresentam desafios como formatos heterogêneos, nomes de parâmetros inconsistentes e necessidade de interpretar limites regulatórios.

Ao integrar validação de dados, cálculo de índices ambientais, verificação de conformidade, visualização e geração automatizada de relatórios em um único fluxo analítico, o **tikatuwq** permite que pesquisadores avancem de forma eficiente de **dados brutos de monitoramento para avaliações ambientais interpretáveis**.

O pacote é particularmente adequado para:
- programas de monitoramento de águas doces  
- avaliações de impacto ambiental  
- pesquisa acadêmica em ciências aquáticas  
- agências ambientais e análises regulatórias  
- séries históricas e bases de dados ambientais de longo prazo  

---

## Dados reais incluídos: Rio Buranhem - INEMA

Este pacote inclui um conjunto real de dados de qualidade da água, extraídos de campanhas de monitoramento do INEMA (Instituto do Meio Ambiente e Recursos Hídricos da Bahia) na bacia do Rio Buranhem (Porto Seguro, Bahia) entre 2021 e 2024. Os dados trazem datas de amostragem, locais (pontos) e variáveis físico-químicas coletadas em campo. Veja a documentação de `wq_demo` para detalhes sobre colunas e exemplos de uso.

A documentação principal e os vignettes usam este conjunto representativo para reproducibilidade.

## Fluxo básico de uso

Um fluxo de análise típico com o **tikatuwq** segue um pipeline reprodutível:

`read_wq → validate_wq → cálculo de índices → verificação regulatória → visualização → relatório`

```r
library(tikatuwq)
data(wq_demo)
head(wq_demo)

# Exemplo típico
wq_demo |> validate_wq() |> iqa(allow_partial = TRUE) |> plot_iqa()

# Auditoria detalhada dos 9 subíndices analíticos do IQA CETESB
iqa_components(wq_demo)

# Visualização do estado trófico
wq_demo |> iet_carlson(.keep_ids = TRUE) |> plot_iet(method = "carlson")

# Análise sazonal
wq_demo |>
  assign_season(region = "bahia") |>
  compare_seasons(param = "turbidez", by = "ponto")
```

---

### 📚 Projeto e Contexto Institucional

O pacote **TikatuWQ** foi desenvolvido por **Vinícius Saraiva Santos** (autor e mantenedor)  
como parte do **Projeto de Pesquisa Tikatu**, conduzido no âmbito do **Núcleo de Pesquisas em Ecossistemas Tropicais – NuPEcoTropic**, grupo de pesquisa vinculado à **Universidade Federal do Sul da Bahia (UFSB)** e coordenado pelo **Prof. Dr. Fabrício Berton Zanchi**.  

Este trabalho foi realizado no contexto das atividades do **Programa de Pós-Graduação em Biossistemas (PPG Biossistemas)** da UFSB, sob orientação do **Prof. Dr. Fabrício Berton Zanchi**.  

O **Projeto Tikatu**, desenvolvido e coordenado por **Vinícius Saraiva Santos**, integra pesquisas voltadas ao monitoramento e modelagem ambiental.

---

## 🆕 Novidades

### 🆕 Novidades na versão 0.10.0 (atual)

**🔬 Auditoria Científica e Metrológica do IQA Brasileiro (CETESB/INEMA)**

- **Equações Analíticas Contínuas Oficiais**: `iqa()` implementa as equações analíticas paramétricas por trechos oficiais da CETESB para todos os 9 sub-índices ($Q_1$ a $Q_9$), incluindo saturação de OD ($C_s$) corrigida por altitude e temperatura, equações em base exponencial natural ($\text{DBO}_5$, $\text{NT}$, $\text{Turb}$, $\text{PT}$) e polinômios estritos.
- **Conversão Estequiométrica de Fósforo**: Conversão automática de fósforo elementar para fosfato ($\text{P}_{\text{total}} \times 3.066 = \text{PO}_4$) quando `phosphorus_basis = "P"`.
- **Fator E. coli ($1.25\times$)**: Suporte nativo a `microbial_type = "e_coli"`, aplicando o fator de conversão oficial da CETESB.
- **Separação Estrita de Sólidos Totais vs. TDS**: Sólidos Dissolvidos Totais (`tds`) não é mais aceito como alias de Sólidos Totais (`solidos_totais`/`residuo_total`) no modo CETESB estrito, prevenindo superestimativa de qualidade da água.
- **Camada de Auditoria dos Componentes**: Nova função exportada `iqa_components()` e opção `iqa(..., details = TRUE)` retornando valores brutos, sub-índices analíticos ($Q_i$), pesos oficiais ($W_i$) e parcelas ponderadas ($Q_i^{W_i}$) para reprodutibilidade científica total.
- **Faixas Qualitativas Oficiais CETESB**: `classify_iqa()` atualizado para os limiares oficiais da CETESB: *Péssima* ($\le 19$), *Ruim* ($19\text{--}36$), *Regular* ($36\text{--}51$), *Boa* ($51\text{--}79$), *Ótima* ($> 79$).
- **Contexto Ambiental em CONAMA**: `conama_check()` passa a receber `environment = c("all", "lotic", "lentic", "intermediate")` para seleção automática dos limites legais de fósforo total e amônia total condicionada ao pH.
- **Validação com Benchmark Oficial**: Validado contra dados oficiais de monitoramento do INEMA no Rio Buranhém (Campanha 3 de 2024).

✔️ `R CMD check --as-cran`: **0 errors | 0 warnings | 0 notes**  
✔️ **299 testes unitários aprovados**  
✔️ Compatível com CRAN, Windows, Linux e macOS

---

### 🆕 Novidades na versão 0.9.0

**⚠️ Mudança incompatível — IQA agora usa a média geométrica ponderada correta**

`iqa()` passa a usar por padrão `method = "CETESB"`, que calcula a média geométrica ponderada `∏(Qi^Wi)` conforme a metodologia da CETESB e a formulação original do NSF WQI (Brown et al., 1970). O comportamento anterior (média aritmética, incorreto) é preservado via `method = "NSF_approx"`. Usuários que dependem do valor padrão obterão resultados mais precisos — e em geral ligeiramente menores.

**Novas funções na v0.9.0:**

- `conama_freq_check()` — implementa a regra de frequência legal do Art. 15 da CONAMA 357/2005 (≥ 80% de ao menos 6 amostras por ano).
- `assign_season()` — classifica cada amostra como `"chuvoso"` ou `"seco"` com base em calendários hidrológicos regionais brasileiros.
- `compare_seasons()` — compara parâmetros entre os períodos chuvoso e seco.
- `plot_iet()` — gráfico de barras do Índice de Estado Trófico.
- `compute_load()` — calcula carga poluidora (kg/dia, t/dia, kg/ano, g/s).
- `exceedance_prob()` — probabilidade empírica de excedência com IC de Wilson.
- `wq_pca()` — wrapper PCA multivariado com biplot, screeplot e loadings.
- `nsfwqi()` — atualizado com agregação geométrica e suporte multilíngue.

---

## Instalação

```r
## Instale a versão atual a partir do CRAN:

install.packages("tikatuwq")

## Instalação via GitHub

install.packages("remotes")

# development version
remotes::install_github("tikatuwq/tikatuwq", dependencies = TRUE)

# versão estável (por tag)
remotes::install_github("tikatuwq/tikatuwq@v0.10.0", build_vignettes = TRUE)
```

---

### Conformidade CONAMA (classe 2)

```r
df <- read_wq(system.file("extdata","exemplo_chamagunga.csv", package="tikatuwq"))

# Tabela apenas com não conformidades, pronta para relatório
conama_report(df, "2", only_violations = TRUE, pretty = TRUE)

# Resumo textual curto
cat(paste(conama_text(df, "2", only_violations = TRUE), collapse = "\n"))

# Regra de frequência legal (CONAMA 357/2005 Art. 15)
conama_freq_check(df, classe = "2", by = "ponto")
```

---

## Principais funções

**Índices de qualidade da água**
- `iqa(df, method = "CETESB", na_rm = TRUE, ...)` — Índice de Qualidade da Água; média geométrica ponderada (CETESB/NSF por padrão).
- `iet_carlson(df)` / `iet_lamparelli(df)` — Índice do Estado Trófico.
- `nsfwqi(df, na_rm = TRUE, add_status = TRUE)` — NSF WQI com agregação por média geométrica.

**Conformidade CONAMA e balneabilidade**
- `conama_limits(class)` — limites da Resolução CONAMA 357/2005.
- `conama_check(df, class)` — verificação de conformidade por parâmetro (colunas `*_ok`).
- `conama_freq_check(df, classe, by)` — regra de frequência Art. 15 (≥ 80% de conformidade em ≥ 6 amostras/ano).
- `balnear_check(df, by)` — classificação de balneabilidade pela CONAMA 274/2000 (Excelente → Imprópria).

**Análise sazonal e tendência**
- `assign_season(df, region)` — classifica amostras por período hidrológico usando calendários regionais.
- `compare_seasons(df, param, test)` — comparação estatística entre períodos chuvoso e seco.
- `mk_seasonal(df, param, period)` — teste de Mann-Kendall sazonal (Hirsch et al., 1982); retorna Z, p-valor, tau e inclinação de Sen.

**Carga e risco**
- `compute_load(df, param, flow_col, unit_out)` — carga poluidora (kg/dia, t/dia, etc.).
- `exceedance_prob(df, param, threshold, direction, by)` — probabilidade empírica de excedência com IC de Wilson.

**Multivariado**
- `wq_pca(df, params, color_by)` — PCA com biplot, screeplot e gráfico de loadings.

**Visualizações**
- `plot_iqa()`, `plot_iet()`, `plot_series()`, `plot_box()`, `plot_heatmap()`, `plot_trend()` — retornam objetos `ggplot`.
- `plot_map()`, `plot_map_quality()` — mapas interativos Leaflet; `plot_map_quality()` colore os pontos pela classe do IQA/IET/NSF WQI.

**Dados e relatórios**
- `read_wq(path)` — lê conjuntos de dados de qualidade da água (CSV).
- `validate_wq(df)` — valida e normaliza colunas e unidades.
- `generate_analysis()`, `render_report()` — relatórios textuais e documentais automáticos.

---

## Documentação e suporte

- **Página CRAN:** https://cran.r-project.org/package=tikatuwq  
- **Site pkgdown:** https://tikatuwq.github.io/tikatuwq/  
- **Sugestões e issues:** https://github.com/tikatuwq/tikatuwq/issues  
- **Releases:** https://github.com/tikatuwq/tikatuwq/releases  

---

### Como citar

Se você utilizar o **tikatuwq** em sua pesquisa ou projeto, por favor cite:

> Santos, V. S. (2025). *tikatuwq: Avaliação da Qualidade da Água e Conformidade Ambiental no Brasil* (v0.10.0). Zenodo. [https://doi.org/10.5281/zenodo.17407297](https://doi.org/10.5281/zenodo.17407297)

Entrada BibTeX:

```bibtex
@Manual{Santos2025tikatuwq,
  title  = {tikatuwq: Water Quality Assessment and Environmental Compliance in Brazil},
  author = {Vinicius Saraiva Santos},
  year   = {2025},
  note   = {R package version 0.10.0},
  doi    = {10.5281/zenodo.17407297},
  url    = {https://github.com/tikatuwq/tikatuwq},
}
```

# 💧 TikatuWQ: Um Pacote R para Avaliação da Qualidade da Água e Conformidade Ambiental no Brasil

**Fluxos de trabalho reprodutíveis para monitoramento de águas doces no Brasil, incluindo índices de qualidade da água, verificações regulatórias e geração automatizada de relatórios.**

**TikatuWQ** é um pacote R de código aberto desenvolvido para analisar, visualizar e relatar dados de qualidade da água de acordo com os padrões ambientais brasileiros.  
Implementa os principais índices utilizados no país: **IQA/NSFWQI** e **IET (Carlson e Lamparelli)** e realiza verificações automáticas de conformidade com a **Resolução CONAMA 357/2005** — incluindo a regra de frequência legal (Art. 15).  
O pacote também inclui análise sazonal, cálculo de carga poluidora, probabilidade de excedência, PCA multivariado, análise de tendências, validação de dados e geração automática de relatórios.

📄 [Read in English](https://github.com/tikatuwq/tikatuwq/blob/main/README.md)

[![CRAN status](https://www.r-pkg.org/badges/version/tikatuwq)](https://cran.r-project.org/package=tikatuwq) 
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/tikatuwq)](https://cran.r-project.org/package=tikatuwq) 
[![CRAN monthly downloads](https://cranlogs.r-pkg.org/badges/tikatuwq)](https://cran.r-project.org/package=tikatuwq) 
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17407296.svg)](https://doi.org/10.5281/zenodo.17407296) 
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

### 🆕 Novidades na versão 0.11.0 (atual)

**Os resultados do IQA mudam para algumas entradas.** As equações dos
sub-índices de pH (8,5–10), DBO (5–15 mg/L) e nitrogênio total (10–60 mg/L)
foram corrigidas para eliminar descontinuidades encontradas na v0.10.0, após
comparação com as curvas do Apêndice D da CETESB (2023, Figura 1). A CETESB
publica os sub-índices apenas como curvas gráficas; as equações do pacote são
aproximações analíticas delas. Veja o
`NEWS.md` e `system.file("audit", "IQA_AUDITORIA.md", package = "tikatuwq")`
para a comparação antes/depois e a lista de pendências.

**Os valores de estado trófico também mudam.** O `iet_lamparelli()` passa a
usar as equações de Lamparelli (2004) apresentadas pela CETESB (Apêndice D
2023, p. 10), diferentes para rios e reservatórios; o `iet_carlson()` passa a
usar o logaritmo natural, como definido por Carlson (1977).

- `nsfwqi()` é documentado como aproximação; com `na_rm = FALSE`, um componente ausente agora resulta em `NA`.
- `classify_iqa()` usa `classification` (como `iqa()`); `scheme` foi depreciado.
- `classify_iqa()` oferece as escalas `"cetesb"`, `"igam"` e `"ana"` (mesmos cortes da `"cetesb"`); elas mudam apenas os rótulos, nunca o valor do IQA. Em `iqa()` a escala segue `method`; o argumento `classification` de `iqa()` foi removido (mudança incompatível com a 0.10.0): use `classify_iqa()`.
- `iqa(df, method = "IGAM")` calcula o IQA do IGAM (Minas Gerais) com motor próprio (veja abaixo).
- `iqa_methods()` lista os motores de cálculo e o escopo de validação de cada um.
- `compare_iqa(df, methods = c("CETESB", "IGAM"))` compara os dois motores em amostras pareadas.

#### IQA-IGAM: implementação e validação

O `tikatuwq` implementa uma reprodução computacional documentada do IQA adotado
pelo Instituto Mineiro de Gestão das Águas (IGAM). Ela se baseia no SCQA
(2005), na documentação institucional e na literatura acadêmica.

- **Turbidez:** o subíndice usa o coeficiente `1,5` no termo trigonométrico,
  registrado por Bucci (2011, p. 51) e Nunes (2008, Quadro 10). Cópias do SCQA
  trazem `15`.
- **Resultados:** a escolha foi respaldada pela reprodução dos resultados
  publicados pelo IGAM. Os **49 registros** de três campanhas na sub-bacia da
  Pampulha (2017–2018) coincidem com o valor publicado após arredondar a uma
  casa decimal. O maior erro absoluto sem arredondamento é 0,048876. Quinze
  desses registros serviram à investigação; os outros 34 foram conferidos com
  critérios pré-registrados.
- **Dados não redistribuídos:** os 49 registros históricos são dados do IGAM e
  não acompanham o pacote. Só as métricas agregadas são documentadas, com as
  fontes e o procedimento de reprodução. Os testes do pacote usam dados
  sintéticos.
- **Sem homologação:** a implementação **não é homologada pelo IGAM**.
- **Não coberto:** outras bacias, nitrato acima do limite de detecção e as
  regras do IGAM para valores censurados, cloreto ausente e arredondamento.
- **Entradas:** o IGAM exige cloreto (saturação de OD) e unidade explícita do
  nitrato (`nitrate_unit = "N"` ou `"NO3"`). O fósforo total (`p_total`) exige
  `phosphorus_basis`.

Equações, fontes (com páginas), requisitos de entrada, métricas de validação e
o procedimento de reprodução estão em `system.file("methodology", "IGAM_VALIDATION.md", package = "tikatuwq")`.

```r
# Amostra sintética; nitrato em mg/L N e fósforo total em mg/L P são declarados
g <- data.frame(od = 7, temperatura = 25, cloreto = 10, e_coli = 100, ph = 7.3,
                dbo = 2, nitrato = 1, p_total = 0.1, turbidez = 5, solidos_totais = 100)
iqa(g, method = "IGAM", altitude_m = 0, nitrate_unit = "N", phosphorus_basis = "P")
#> IQA 75,5 (Bom)
```

Comparação dos dois motores nas mesmas amostras (dados ilustrativos; só as linhas
válidas nos dois métodos entram nas estatísticas). Os métodos não usam os mesmos
parâmetros: o CETESB usa nitrogênio total e fósforo total; o IGAM usa nitrato,
fosfato total e cloreto. As classes também têm cortes diferentes; portanto, as
diferenças refletem a metodologia, não erro de medição:

```r
d <- data.frame(
  ponto = c("P1", "P2", "P3", "P4"),
  od = c(7, 5, 3, 6), temperatura = 24, cloreto = c(10, 30, 60, NA),
  coliformes = c(100, 2000, 20000, 500), ph = c(7.2, 7.5, 7, 7.4),
  dbo = c(2, 5, 15, 3), nt_total = c(1, 3, 8, 2), nitrato = c(0.5, 1, 2, 0.8),
  p_total = c(0.05, 0.2, 0.8, 0.1), turbidez = c(5, 20, 60, 10),
  solidos_totais = c(100, 200, 400, 150)
)
cmp <- compare_iqa(d, methods = c("CETESB", "IGAM"), id_cols = "ponto",
                   phosphorus_basis = "P", nitrate_unit = "N", altitude_m = 800)
cmp[, c("ponto", "IQA_CETESB", "IQA_IGAM", "par_valido", "motivo_IGAM")]
attr(cmp, "summary")            # estatísticas pareadas (3 pares válidos aqui)
attr(cmp, "class_transitions")  # classe CETESB -> classe IGAM (escalas diferentes)
```
- `iqa(details = TRUE)` informa os pesos efetivamente usados, inclusive com `pesos` customizados e `allow_partial = TRUE`.
- `delta_temperature` agora é repassado; valores físicos inválidos viram `NA` com aviso.

✔️ `R CMD check --as-cran`: **0 errors | 0 warnings** no Windows, Linux e macOS (o win-builder aponta 1 nota: o nome "Minas Gerais" marcado como possível erro de grafia)  
✔️ **181 testes automatizados (806 verificações) aprovados**  
✔️ Verificado no Windows (local, win-builder R-devel e R-release), Linux (Ubuntu) e macOS (R-release, Apple silicon)

---

### 🆕 Novidades na versão 0.10.0

**🔬 Auditoria Científica e Metrológica do IQA Brasileiro (CETESB/INEMA)**

- **Equações Analíticas**: `iqa()` implementa equações analíticas por trechos para os 9 sub-índices da CETESB ($Q_1$ a $Q_9$), incluindo saturação de OD ($C_s$) corrigida por altitude e temperatura. *Nota: a auditoria de 2026 encontrou erros em alguns trechos (corrigidos na v0.11.0).*
- **Conversão Estequiométrica de Fósforo**: Conversão automática de fósforo elementar para fosfato ($\text{P}_{\text{total}} \times 3.066 = \text{PO}_4$) quando `phosphorus_basis = "P"`.
- **Fator E. coli ($1.25\times$)**: Suporte nativo a `microbial_type = "e_coli"`, aplicando um fator de conversão de 1,25 (Apêndice D CETESB 2023, p. 5).
- **Separação Estrita de Sólidos Totais vs. TDS**: Sólidos Dissolvidos Totais (`tds`) não é mais aceito como alias de Sólidos Totais (`solidos_totais`/`residuo_total`) no modo CETESB estrito, prevenindo superestimativa de qualidade da água.
- **Camada de Auditoria dos Componentes**: Nova função exportada `iqa_components()` e opção `iqa(..., details = TRUE)` retornando valores brutos, sub-índices analíticos ($Q_i$), pesos oficiais ($W_i$) e parcelas ponderadas ($Q_i^{W_i}$) para reprodutibilidade científica total.
- **Faixas Qualitativas Oficiais CETESB**: `classify_iqa()` atualizado para os limiares oficiais da CETESB: *Péssima* ($\le 19$), *Ruim* ($19\text{--}36$), *Regular* ($36\text{--}51$), *Boa* ($51\text{--}79$), *Ótima* ($> 79$).
- **Contexto Ambiental em CONAMA**: `conama_check()` passa a receber `environment = c("all", "lotic", "lentic", "intermediate")` para seleção automática dos limites legais de fósforo total e amônia total condicionada ao pH.
- **Dados de exemplo do INEMA**: Caso de teste montado com dados de monitoramento do INEMA no Rio Buranhém (Campanha 3 de 2024). Verifica a classe e uma faixa plausível; não compara com valores de IQA publicados pelo INEMA.

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

# uma versão específica por tag (a última tag no GitHub é v0.8.2; para versões posteriores use o CRAN)
remotes::install_github("tikatuwq/tikatuwq@v0.8.2", build_vignettes = TRUE)
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
- `iqa(df, method = "CETESB", ...)` — Índice de Qualidade da Água; média geométrica ponderada (CETESB por padrão). `method = "IGAM"` usa o motor do IGAM (Minas Gerais).
- `iqa_components(df)` — subíndices, pesos e parcelas ponderadas de cada amostra (CETESB).
- `iqa_methods()` — motores de cálculo disponíveis e o escopo de validação de cada um.
- `compare_iqa(df, methods = c("CETESB", "IGAM"))` — comparação pareada entre motores.
- `classify_iqa(x, classification)` — classes do IQA nas escalas CETESB, IGAM ou ANA.
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

> Santos, V. S. (2026). *tikatuwq: Avaliação da Qualidade da Água e Conformidade Ambiental no Brasil* (pacote R versão 0.11.0). Zenodo (DOI de conceito, todas as versões). [https://doi.org/10.5281/zenodo.17407296](https://doi.org/10.5281/zenodo.17407296)

Entrada BibTeX:

```bibtex
@Manual{Santos2026tikatuwq,
  title  = {tikatuwq: Water Quality Assessment and Environmental Compliance in Brazil},
  author = {Vinicius Saraiva Santos},
  year   = {2026},
  note   = {R package version 0.11.0},
  doi    = {10.5281/zenodo.17407296},
  url    = {https://github.com/tikatuwq/tikatuwq},
}
```

O DOI é o DOI de *conceito* do Zenodo, que aponta para a versão depositada mais recente (hoje a v0.7.0); a versão 0.11.0 ainda não tem depósito próprio. `citation("tikatuwq")` traz a mesma referência.

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

**TikatuWQ** é um pacote R de código aberto desenvolvido para analisar, visualizar e relatar dados de qualidade da água de acordo com os padrões ambientais brasileiros. Ele implementa os principais índices utilizados no país, **IQA/NSFWQI** e **IET (Carlson e Lamparelli)**, e fornece verificações automatizadas de conformidade com a **Resolução CONAMA 357/2005**. O pacote também inclui análise de tendências, validação de dados e geração automática de relatórios.

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
wq_demo |> validate_wq() |> iqa(na_rm = TRUE) |> plot_iqa()

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

### 🆕 Novidades na versão 0.9.0 (atual)

**⚠️ Mudança incompatível — IQA agora usa a média geométrica ponderada correta**

`iqa()` passa a usar por padrão `method = "CETESB"`, que calcula a média geométrica ponderada `∏(Qi^Wi)` conforme a metodologia da CETESB e a formulação original do NSF WQI (Brown et al., 1970). O comportamento anterior (média aritmética, incorreto) é preservado via `method = "NSF_approx"`. Usuários que dependem do valor padrão obterão resultados mais precisos — e em geral ligeiramente menores.

**Novas funções:**

- `conama_freq_check()` — implementa a regra de frequência legal do Art. 15 da CONAMA 357/2005: um parâmetro é considerado conforme apenas quando ≥ 80% de ao menos 6 amostras por ano estão dentro dos limites. Retorna uma tabela de conformidade por ponto, ano e parâmetro.
- `assign_season()` — classifica cada amostra como `"chuvoso"` ou `"seco"` com base em calendários hidrológicos regionais brasileiros (Sudeste, Nordeste, Norte, Sul, Centro-Oeste, Bahia) ou em um vetor de meses personalizado.
- `compare_seasons()` — compara um parâmetro de qualidade da água entre os períodos chuvoso e seco usando Wilcoxon, teste t ou Kruskal-Wallis; retorna estatísticas descritivas, resultado do teste e um boxplot `ggplot` opcional.
- `plot_iet()` — gráfico de barras (vertical ou horizontal) do Índice de Estado Trófico com coloração por classe trófica, compatível com os esquemas de Carlson (1977) e Lamparelli (2004).
- `compute_load()` — calcula a carga poluidora como concentração × vazão × fator de unidade; saídas em kg/dia, t/dia, kg/ano e g/s.
- `exceedance_prob()` — estima a probabilidade empírica de excedência com intervalo de confiança de Wilson, por grupo.
- `wq_pca()` — wrapper PCA sobre `stats::prcomp()` com seleção automática de colunas, biplot, screeplot e gráfico de loadings retornados como atributos `ggplot`.
- `nsfwqi()` — atualizado: passa a usar média geométrica ponderada (consistente com a correção do IQA); adiciona argumentos `add_status` e `locale` para rótulos de status multilíngues.

**Tabela de limites expandida:**

- `inst/extdata/conama_limits.csv` ampliada de ~38 para ~116 linhas, incluindo espécies de nitrogênio (NO₃, NO₂, NH₃ com limites condicionais ao pH), íons inorgânicos (fluoretos, cloretos, sulfatos), poluentes orgânicos (fenóis, surfactantes) e 14 metais pesados para as Classes 1–3.

✔️ `R CMD check --as-cran`: **0 errors | 0 warnings | 0 notes**  
✔️ Compatível com CRAN, Windows, Linux e macOS

---

### 🆕 Novidades na versão 0.8.2

- Manutenção CRAN: corrigido o exemplo de `plot_map()` para usar o dataset interno `wq_demo` em vez de referência a arquivo externo. Todos os exemplos e testes estão em conformidade com as políticas do CRAN.

### 🆕 Novidades na versão 0.8.1

- Ajustes internos para garantir plena conformidade com as políticas do CRAN em relação ao acesso ao sistema de arquivos.
- Exemplos e documentação passam a depender exclusivamente do dataset interno `wq_demo`, removendo qualquer dependência de arquivos externos ou locais.
- A função `render_report()` agora grava saída **somente em diretórios temporários** (`tempdir()`) ou em diretórios explicitamente fornecidos pelo usuário.
- Exemplos, testes e documentação foram revisados para garantir execução segura em ambientes restritos (ex.: sistemas de verificação do CRAN).
- Sem alterações na API e sem impacto funcional para os usuários.

✔️ `R CMD check --as-cran`: **0 errors | 0 warnings | 0 notes**  
✔️ Compatível com CRAN, Windows, Linux e macOS

### 🆕 Novidades na versão 0.8.0

- O dataset de exemplo `wq_demo` agora é um subconjunto real de dados (INEMA, Rio Buranhem, Porto Seguro-BA, 2021–2024), com 20 linhas e 14 colunas (incluindo `rio`, `lat`, `lon`).
- Exemplos e vignettes passam a usar este conjunto real para maior reprodutibilidade e clareza.
- Documentação atualizada (help do dataset, README, vignette) para refletir a mudança.
- Sem quebra de API; comportamento permanece compatível com versões anteriores.

### Novidades na versão 0.7.3

- IQA mais robusto:
aceita `temp` como alias de `temperatura`.
"Numificação" automática de valores com vírgula decimal e sinais `<`/`>`.
Com `na_rm = TRUE`, repondera os pesos quando faltarem parâmetros.
- IET (Carlson / Lamparelli) com data.frame:
`iet_carlson()` e `iet_lamparelli()` agora aceitam um data.frame com colunas como `rio`, `ponto`, `data`, `lat`, `lon`.
Parâmetros relevantes são detectados automaticamente (`secchi/sd`, `clorofila/chla`, `tp/p_total`).
`p_total` em mg/L é convertido automaticamente para `tp` em µg/L.
Use `.keep_ids = TRUE` para preservar identificadores na saída.
Sem novas dependências, sem quebra de API.

### Novidades v0.7.2

- Correção de *NOTE* nos testes de pré-submissão do CRAN:
  - Remoção dos campos não padrão (`DOI`, `Citation`) do arquivo `DESCRIPTION`.
  - Atualização do arquivo `inst/CITATION` para o formato `bibentry()` (substituindo `citEntry()`).
- Nenhuma alteração funcional — o comportamento do pacote permanece o mesmo.

### Novidades v0.7.0 
- Novas funções **`param_analysis()`** e **`param_analysis_multi()`**:
  - Permitem análises flexíveis por parâmetro e ponto de coleta ou rio.
  - Suportam comparações cruzadas (múltiplos parâmetros por ponto e vice-versa).
  - Incluem estatísticas descritivas e detecção de tendências temporais.
- Cobertura total de testes para os novos módulos.
- Pequenas melhorias em `plot_trend()` e `plot_map()` (mensagens controladas).
- Todas as validações CRAN e `devtools::check()` passaram sem erros.

### Novidades v0.6.2
- Atualização corretiva solicitada pelo CRAN.
- Corrigido o aviso **codoc** na documentação de `generate_analysis()`.
- Removidos parâmetros obsoletos `id_cols` e `filter` para manter compatibilidade com a versão atual.
- Nenhuma alteração funcional no código.

### Novidades v0.6.1 
- Atualização de manutenção solicitada pelo CRAN.
- Corrigido o link relativo `README-pt.md`, agora convertido para URL HTTPS absoluta.
- Nenhuma alteração funcional no código.

### Novidades v0.6.0
- Nova função `plot_trend()` para análise de tendências temporais:
  - Linhas de tendência por parâmetro/ponto com métodos **Theil-Sen**, **OLS** e **LOESS**.
  - Suporte a facetas por rio/ponto e personalização de pontos.
  - Retorna um objeto `ggplot` pronto para visualização ou relatórios.
- Documentação e exemplos atualizados no site pkgdown.

### v0.5.1
- Corrigidos **URLs inválidos** reportados pelo CRAN (links e DOIs atualizados).
- Pequenos ajustes de documentação para compatibilidade com o R-devel.

### v0.5.0
- Adicionadas funções **helper internas** para simplificar o fluxo de trabalho.
- Novo recurso `plot_map()` para visualização espacial dos pontos de amostragem.
- Mensagens de validação revisadas e formatação padronizada.

### v0.2.1
- `generate_analysis()` — geração automática de parágrafos analíticos baseados em regras.
- Modelo de relatório atualizado para incluir análise textual.
- Estruturas adicionadas para `iet_lamparelli()` e `nsfwqi()`.

---

## Instalação

```r
## Instale a versão atual a partir do CRAN:

install.packages("tikatuwq")

## Instalação via GitHub

```r
install.packages("remotes")

# development version
remotes::install_github("tikatuwq/tikatuwq", dependencies = TRUE)

# versão estável (por tag)
remotes::install_github("tikatuwq/tikatuwq@v0.9.0", build_vignettes = TRUE)
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

Se você utilizar o **tikatuwq** em suas pesquisas, cite da seguinte forma:

> Santos, V. S. (2025). *tikatuwq: Avaliação da Qualidade da Água e Conformidade Ambiental no Brasil* (v0.9.0). Zenodo. [https://doi.org/10.5281/zenodo.17407297](https://doi.org/10.5281/zenodo.17407297)

Entrada BibTeX:

```bibtex
@Manual{Santos2025tikatuwq,
  title  = {tikatuwq: Avaliação da Qualidade da Água e Conformidade Ambiental no Brasil},
  author = {Vinicius Saraiva Santos},
  year   = {2025},
  note   = {R package version 0.9.0},
  doi    = {10.5281/zenodo.17407297},
  url    = {https://github.com/tikatuwq/tikatuwq},
}
```

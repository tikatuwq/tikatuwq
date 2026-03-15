# 💧 TikatuWQ: Um Pacote R para Avaliação da Qualidade da Água e Conformidade Ambiental no Brasil

**Fluxos de trabalho reprodutíveis para monitoramento de águas doces no Brasil, incluindo índices de qualidade da água, verificações regulatórias e geração automatizada de relatórios.**

[![CRAN status](https://www.r-pkg.org/badges/version/tikatuwq)](https://cran.r-project.org/package=tikatuwq) 
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/tikatuwq)](https://cran.r-project.org/package=tikatuwq) 
[![CRAN monthly downloads](https://cranlogs.r-pkg.org/badges/tikatuwq)](https://cran.r-project.org/package=tikatuwq) 
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17407297.svg)](https://doi.org/10.5281/zenodo.17407297) 
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) 
[![R-CMD-check](https://github.com/tikatuwq/tikatuwq/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tikatuwq/tikatuwq/actions) 
![Status do manuscrito](https://img.shields.io/badge/manuscrito-em_avaliação-yellow) 

**TikatuWQ** é um pacote R de código aberto desenvolvido para analisar, visualizar e relatar dados de qualidade da água de acordo com os padrões ambientais brasileiros. Ele implementa os principais índices utilizados no país — **IQA/NSFWQI** e **IET (Carlson e Lamparelli)** — e fornece verificações automatizadas de conformidade com a **Resolução CONAMA 357/2005**. O pacote também inclui análise de tendências, validação de dados e geração automática de relatórios.

---

## Instalação (desenvolvimento)

Para instalar as dependências de desenvolvimento e verificar o pacote localmente:

```r
install.packages(c("devtools","testthat","rmarkdown","ggplot2","dplyr","tidyr",
                   "readr","lubridate","stringr","glue","scales","broom","purrr"))
devtools::load_all("tikatuwq")
devtools::check("tikatuwq")
```

---

## Dados reais incluídos: Rio Buranhem - INEMA

Este pacote inclui agora um conjunto real de dados de qualidade da água, extraídos de campanhas de monitoramento do INEMA (Instituto do Meio Ambiente e Recursos Hídricos da Bahia) na bacia do Rio Buranhem (Porto Seguro, Bahia) entre 2021 e 2024. Os dados trazem datas de amostragem, locais (pontos) e variáveis físico-químicas coletadas em campo. Veja a documentação de `wq_demo` para detalhes sobre colunas e exemplos de uso.

A documentação principal e os vignettes usam este conjunto representativo para reproducibilidade.

## Fluxo básico de uso

```r
library(tikatuwq)
data(wq_demo)
head(wq_demo)
# Exemplo típico
wq_demo |> validate_wq() |> iqa(na_rm = TRUE) |> plot_iqa()
```

---

### 📚 Contexto Científico e Institucional

O pacote **TikatuWQ** foi desenvolvido por **Vinícius Saraiva Santos** (autor e mantenedor)  
como parte do **Projeto de Pesquisa Tikatu**, conduzido no âmbito do **Núcleo de Pesquisas em Ecossistemas Tropicais – NuPEcoTropic**, grupo de pesquisa vinculado à **Universidade Federal do Sul da Bahia (UFSB)** e coordenado pelo **Prof. Dr. Fabrício Berton Zanchi**.  

Este trabalho foi realizado no contexto das atividades do **Programa de Pós-Graduação em Biossistemas (PPG Biossistemas)** da UFSB, sob orientação do **Prof. Dr. Fabrício Berton Zanchi**.  

O **Projeto Tikatu**, desenvolvido e coordenado por **Vinícius Saraiva Santos**, integra pesquisas voltadas ao monitoramento e modelagem ambiental.

---

## 🆕 Novidades

### 🆕 Novidades na versão 0.8.0 (atual)

- O dataset de exemplo `wq_demo` agora é um subconjunto real de dados (INEMA, Rio Buranhem, Porto Seguro-BA, 2021–2024), com 20 linhas e 14 colunas (incluindo `rio`, `lat`, `lon`).
- Exemplos e vignettes passam a usar este conjunto real para maior reprodutibilidade e clareza.
- Documentação atualizada (help do dataset, README, vignette) para refletir a mudança.
- Sem quebra de API; comportamento permanece compatível com versões anteriores.

### Novidades na versão 0.7.3

- IQA mais robusto
Aceita temp como alias de temperatura.
“Numificação” automática de valores com vírgula decimal e sinais < >.
Com na_rm = TRUE, repondera os pesos quando faltarem parâmetros.
- IET (Carlson / Lamparelli) com data.frame
- Agora iet_carlson() e iet_lamparelli() aceitam um data.frame “cru” com colunas como rio, ponto, data, lat, lon, etc.
- Parâmetros relevantes são detectados automaticamente (secchi/sd, clorofila/chla, tp/p_total).
p_total em mg/L é convertido automaticamente para tp em µg/L.
- Use .keep_ids = TRUE para preservar identificadores (ex.: rio, ponto, data) na saída.
Sem novas dependências, sem quebra de API.
Tudo continua funcionando como antes para quem usa as chamadas vetoriais.

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

## Instalação via GitHub

```r
install.packages("remotes")  # ou devtools
remotes::install_github("tikatuwq/tikatuwq", dependencies = TRUE)

# versão estável (por tag)
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
```

---

## Principais funções

- `read_wq(path)` — lê conjuntos de dados de qualidade da água (CSV).  
- `validate_wq(df)` — valida e normaliza colunas e unidades.  
- `iqa(df, na_rm = TRUE, ...)` — Índice de Qualidade da Água (CETESB/NSF).  
- `iet_carlson(df)` / `iet_lamparelli(df)` — Índice do Estado Trófico.  
- `nsfwqi(df)` — NSFWQI (estrutura pronta).  
- `conama_limits(class)` — limites da Resolução CONAMA 357/2005.  
- `conama_check(df, class)` — verificação de conformidade por parâmetro (colunas *_ok).  
- Visualizações: `plot_iqa()`, `plot_series()`, `plot_box()`, `plot_heatmap()`, `plot_map()`, `plot_trend()` (retorna objeto `ggplot`).  
- Relatórios/Textos: `generate_analysis()`, `render_report()`.  
- Dados de exemplo: `system.file("extdata", "exemplo_chamagunga.csv", package = "tikatuwq")`.

---

## Documentação e suporte

- **Página CRAN:** https://cran.r-project.org/package=tikatuwq  
- **Site pkgdown:** https://tikatuwq.github.io/tikatuwq/  
- **Sugestões e issues:** https://github.com/tikatuwq/tikatuwq/issues  
- **Releases:** https://github.com/tikatuwq/tikatuwq/releases  

---

## Citação
```r
citation("tikatuwq")
```

### Como citar

Se você utilizar o **tikatuwq** em suas pesquisas, cite da seguinte forma:

> Santos, V. S. (2025). *tikatuwq: Avaliação da Qualidade da Água e Conformidade Ambiental no Brasil* (v0.8.0). Zenodo. [https://doi.org/10.5281/zenodo.17407297](https://doi.org/10.5281/zenodo.17407297)

Entrada BibTeX:

```bibtex
@Manual{Santos2025tikatuwq,
  title  = {tikatuwq: Avaliação da Qualidade da Água e Conformidade Ambiental no Brasil},
  author = {Vinicius Saraiva Santos},
  year   = {2025},
  note   = {R package version 0.8.0},
  doi    = {10.5281/zenodo.17407297},
  url    = {https://github.com/tikatuwq/tikatuwq},
}
```

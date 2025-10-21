# 🌊 TikatuWQ: Pacote R para Avaliação da Qualidade da Água e Conformidade Ambiental no Brasil

O **TikatuWQ** é um pacote **open-source em R** desenvolvido para analisar, visualizar e gerar relatórios de qualidade da água de acordo com os padrões ambientais brasileiros.  
Implementa os principais índices usados no país **IQA/NSFWQI** e **IET (Carlson e Lamparelli)**   
e oferece verificações automáticas de conformidade com a **Resolução CONAMA 357/2005**.  
O pacote inclui ainda análise de tendências, validação de dados e geração automática de relatórios.

📄 [Read in English](https://github.com/tikatuwq/tikatuwq/blob/main/README.md)


<!-- CRAN status -->
[![CRAN status](https://www.r-pkg.org/badges/version/tikatuwq)](https://cran.r-project.org/package=tikatuwq)

<!-- CRAN logs - downloads -->
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/tikatuwq)](https://cran.r-project.org/package=tikatuwq)
[![CRAN monthly downloads](https://cranlogs.r-pkg.org/badges/tikatuwq)](https://cran.r-project.org/package=tikatuwq)

<!-- License -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- R CMD check results -->
[![R-CMD-check](https://github.com/tikatuwq/tikatuwq/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tikatuwq/tikatuwq/actions)

<!-- Lifecycle -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)


## Instalação (desenvolvimento)
```r
install.packages(c("devtools","testthat","rmarkdown","ggplot2","dplyr","tidyr",
                   "readr","lubridate","stringr","glue","scales","broom","purrr"))
devtools::load_all("tikatuwq")
devtools::check("tikatuwq")
```

## Fluxo básico
```r
df <- read_wq(system.file("extdata","exemplo_chamagunga.csv", package="tikatuwq")) |>
  validate_wq() |>
  iqa(na_rm = TRUE)

plot_iqa(df)

# Texto analítico (sem IA)
pars <- generate_analysis(df, classe_conama = "2",
                          incluir_tendencia = TRUE,
                          parametros_tendencia = c("turbidez","od","pH"),
                          contexto = list(rio="Chamagunga", periodo="2025-07"))
cat(paste(pars, collapse = "\n\n"))
```

---

### 📚 Contexto científico

O pacote **TikatuWQ** foi desenvolvido por **Vinícius Saraiva Santos** (autor e mantenedor)  
no âmbito do **Projeto de Pesquisa Tikatu**, desenvolvido no **Núcleo de Pesquisas em Ecossistemas Tropicais – NuPEcoTropic**,  grupo de pesquisa vinculado à **Universidade Federal do Sul da Bahia (UFSB)** e coordenado pelo **Prof. Dr. Fabrício Berton Zanchi**.  

O trabalho foi realizado como parte das atividades do **Programa de Pós-Graduação em Biossistemas (PPG Biossistemas)** da UFSB, sob orientação do **Prof. Dr. Fabrício Berton Zanchi**.  

O **Projeto Tikatu** é desenvolvido por **Vinícius Saraiva Santos** e integra pesquisas voltadas ao monitoramento e à modelagem de sistemas ambientais.

---

## 🆕 Novidades

### Novidades v0.7.0 (atual)
- Novas funções **`param_analysis()`** e **`param_analysis_multi()`**:
  - Permitem análise flexível por parâmetro e por ponto.
  - Suportam comparações cruzadas (vários parâmetros por ponto ou vice-versa).
  - Incluem estatísticas descritivas e tendências temporais.
- Cobertura de testes 100% para os novos módulos.
- Pequenas melhorias em `plot_trend()` e `plot_map()` (mensagens controladas).
- Todas as verificações do CRAN e `devtools::check()` passaram sem erros.

### Novidades v0.6.2
- Atualização corretiva solicitada pelo CRAN.
- Correção do WARNING **codoc** na documentação da função `generate_analysis()`.
- Removidos os parâmetros obsoletos `id_cols` e `filter` para corresponder à assinatura atual da função.
- Nenhuma alteração funcional ou de código foi realizada.

### Novidades v0.6.1 
- Atualização de manutenção solicitada pelo CRAN.
- Correção do link relativo `README-pt.md`, agora convertido para URL HTTPS absoluto.
- Nenhuma alteração funcional ou de código.

### Novidades v0.6.0
- Nova função `plot_trend()` para análise de tendências temporais:
  - Linhas de tendência por parâmetro/ponto com métodos **Theil-Sen**, **OLS** e **LOESS**.
  - Suporte a facetas por rio/ponto e personalização de pontos.
  - Retorna objeto `ggplot`, pronto para visualização ou inclusão em relatórios.
- Atualização da documentação e exemplos no site pkgdown.

### v0.5.1
- Correção de **links inválidos** reportados pelo CRAN (URLs atualizadas e DOIs incluídos).
- Ajustes menores na documentação `tikatuwq-package.Rd` para compatibilidade com o R-devel.

### v0.5.0
- Inclusão de **helpers** internos para simplificação de fluxo.
- Nova funcionalidade `plot_map()` para visualização espacial dos pontos de coleta.
- Revisão e padronização das mensagens de validação.

### v0.2.1
- `generate_analysis()` — parágrafos automáticos (rule-based).
- Template de relatório atualizado incluindo análise textual.
- Estruturas para `iet_lamparelli()` e `nsfwqi()`.

---

## Instalação via GitHub

```r
install.packages("remotes")  # ou devtools
remotes::install_github("tikatuwq/tikatuwq", dependencies = TRUE)

# versão estável (por tag)
remotes::install_github("tikatuwq/tikatuwq@v0.5.1", build_vignettes = TRUE)
```

---

### Conformidade CONAMA (classe 2)
```r
df <- read_wq(system.file("extdata","exemplo_chamagunga.csv", package="tikatuwq"))

# Tabela só com infrações, pronta para laudo
conama_report(df, "2", only_violations = TRUE, pretty = TRUE)

# Resumo textual curto
cat(paste(conama_text(df, "2", only_violations = TRUE), collapse = "\n"))
```

---

## Principais funções

- `read_wq(path)` — leitura de dados de qualidade da água (CSV).
- `validate_wq(df)` — validação/normalização de colunas e unidades.
- `iqa(df, na_rm = TRUE, ...)` — IQA (CETESB/NSF).
- `iet_carlson(df)` / `iet_lamparelli(df)` — Índice do Estado Trófico.
- `nsfwqi(df)` — NSFWQI (estrutura pronta).
- `conama_limits(classe)` — limites da Res. CONAMA 357/2005.
- `conama_check(df, classe)` — conformidade por parâmetro (*_ok).
- Visualizações: `plot_iqa()`, `plot_series()`, `plot_box()`, `plot_heatmap()`, `plot_map()`, `plot_trend()`.
- Relatórios/texto: `generate_analysis()`, `render_report()`.
- Dados de exemplo: `system.file("extdata", "exemplo_chamagunga.csv", package = "tikatuwq")`.

---

## Documentação e suporte

- Site (pkgdown): https://tikatuwq.github.io/tikatuwq/  
- Issues/sugestões: https://github.com/tikatuwq/tikatuwq/issues  
- Releases: https://github.com/tikatuwq/tikatuwq/releases  

---

## Como citar
```r
citation("tikatuwq")
```

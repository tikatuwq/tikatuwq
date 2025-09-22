# Tikatu Water Quality (tikatuwq)

Pacote R para análises de qualidade da água no contexto brasileiro: IQA, IET (Carlson/Lamparelli), NSFWQI, limites CONAMA 357/2005, visualizações, relatório e texto analítico automático (rule-based).

## Instalação (desenvolvimento)
```r
install.packages(c("devtools","testthat","rmarkdown","ggplot2","dplyr","tidyr","readr","lubridate","stringr","glue","scales","broom","purrr"))
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
cat(paste(pars, collapse = "

"))
```

## Novidades v0.2.1
- `generate_analysis()` — parágrafos automáticos (rule-based).
- Template de relatório atualizado incluindo análise textual.
- Estruturas para `iet_lamparelli()` e `nsfwqi()` (curvas oficiais a incorporar).


## Instalação via GitHub

```r
install.packages("remotes")  # ou devtools
remotes::install_github("tikatuwq/tikatuwq", dependencies = TRUE)

# versão estável (por tag)
remotes::install_github("tikatuwq/tikatuwq@v0.4.1", build_vignettes = TRUE)
```

Badge (opcional):
```
[![R-CMD-check](https://github.com/tikatuwq/tikatuwq/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tikatuwq/tikatuwq/actions/workflows/R-CMD-check.yaml)
```
## Principais funções

- read_wq(path) — leitura de dados de qualidade da água (CSV).
- validate_wq(df) — validação/normalização de colunas e unidades.
- iqa(df, na_rm = TRUE, ...) — IQA (CETESB/NSF).
- iet_carlson(df) / iet_lamparelli(df) — Índice do Estado Trófico.
- nsfwqi(df) — NSFWQI (estrutura pronta).
- conama_limits(classe) — limites da Res. CONAMA 357/2005.
- conama_check(df, classe) — conformidade por parâmetro (*_ok).
- Visualizações: plot_iqa(), plot_series(), plot_box(), plot_heatmap().
- Relatórios/texto: generate_analysis(), render_report().
- Dados de exemplo: system.file("extdata", "exemplo_chamagunga.csv", package = "tikatuwq").

## Documentação e suporte

- Site (pkgdown): https://tikatuwq.github.io/tikatuwq/

- Issues/sugestões: https://github.com/tikatuwq/tikatuwq/issues

- Releases: https://github.com/tikatuwq/tikatuwq/releases

## Como Citar
```r
citation("tikatuwq")
```

[![CRAN status](https://www.r-pkg.org/badges/version/tikatuwq)](https://cran.r-project.org/package=tikatuwq)

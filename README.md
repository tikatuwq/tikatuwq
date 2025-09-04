# tikatuwq

Pacote R para anÃ¡lises de qualidade da Ã¡gua no contexto brasileiro: IQA, IET (Carlson/Lamparelli), NSFWQI, limites CONAMA 357/2005, visualizaÃ§Ãµes, relatÃ³rio e texto analÃ­tico automÃ¡tico (rule-based).

## InstalaÃ§Ã£o (desenvolvimento)
```r
install.packages(c("devtools","testthat","rmarkdown","ggplot2","dplyr","tidyr","readr","lubridate","stringr","glue","scales","broom","purrr"))
devtools::load_all("tikatuwq")
devtools::check("tikatuwq")
```

## Fluxo bÃ¡sico
```r
df <- read_wq(system.file("extdata","exemplo_chamagunga.csv", package="tikatuwq")) |>
  validate_wq() |>
  iqa(na_rm = TRUE)

plot_iqa(df)

# Texto analÃ­tico (sem IA)
pars <- generate_analysis(df, classe_conama = "2",
                          incluir_tendencia = TRUE,
                          parametros_tendencia = c("turbidez","od","pH"),
                          contexto = list(rio="Chamagunga", periodo="2025-07"))
cat(paste(pars, collapse = "

"))
```

## Novidades v0.2.1
- `generate_analysis()` â€” parÃ¡grafos automÃ¡ticos (rule-based).
- Template de relatÃ³rio atualizado incluindo anÃ¡lise textual.
- Estruturas para `iet_lamparelli()` e `nsfwqi()` (curvas oficiais a incorporar).


## InstalaÃ§Ã£o via GitHub

```r
install.packages("remotes")  # ou devtools
remotes::install_github("tikatuwq/tikatuwq", dependencies = TRUE)

# versÃ£o estÃ¡vel (por tag)
remotes::install_github("tikatuwq/tikatuwq@v0.4.1", build_vignettes = TRUE)
```

Badge (opcional):
```
[![R-CMD-check](https://github.com/tikatuwq/tikatuwq/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tikatuwq/tikatuwq/actions/workflows/R-CMD-check.yaml)
```

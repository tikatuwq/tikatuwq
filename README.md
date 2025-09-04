# tikatuwq

Pacote R para anÃƒÂ¡lises de qualidade da ÃƒÂ¡gua no contexto brasileiro: IQA, IET (Carlson/Lamparelli), NSFWQI, limites CONAMA 357/2005, visualizaÃƒÂ§ÃƒÂµes, relatÃƒÂ³rio e texto analÃƒÂ­tico automÃƒÂ¡tico (rule-based).

## InstalaÃƒÂ§ÃƒÂ£o (desenvolvimento)
```r
install.packages(c("devtools","testthat","rmarkdown","ggplot2","dplyr","tidyr","readr","lubridate","stringr","glue","scales","broom","purrr"))
devtools::load_all("tikatuwq")
devtools::check("tikatuwq")
```

## Fluxo bÃƒÂ¡sico
```r
df <- read_wq(system.file("extdata","exemplo_chamagunga.csv", package="tikatuwq")) |>
  validate_wq() |>
  iqa(na_rm = TRUE)

plot_iqa(df)

# Texto analÃƒÂ­tico (sem IA)
pars <- generate_analysis(df, classe_conama = "2",
                          incluir_tendencia = TRUE,
                          parametros_tendencia = c("turbidez","od","pH"),
                          contexto = list(rio="Chamagunga", periodo="2025-07"))
cat(paste(pars, collapse = "

"))
```

## Novidades v0.2.1
- `generate_analysis()` Ã¢â‚¬â€ parÃƒÂ¡grafos automÃƒÂ¡ticos (rule-based).
- Template de relatÃƒÂ³rio atualizado incluindo anÃƒÂ¡lise textual.
- Estruturas para `iet_lamparelli()` e `nsfwqi()` (curvas oficiais a incorporar).


## InstalaÃƒÂ§ÃƒÂ£o via GitHub

```r
install.packages("remotes")  # ou devtools
remotes::install_github("tikatuwq/tikatuwq", dependencies = TRUE)

# versÃƒÂ£o estÃƒÂ¡vel (por tag)
remotes::install_github("tikatuwq/tikatuwq@v0.4.1", build_vignettes = TRUE)
```

Badge (opcional):
```
[![R-CMD-check](https://github.com/tikatuwq/tikatuwq/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tikatuwq/tikatuwq/actions/workflows/R-CMD-check.yaml)
```

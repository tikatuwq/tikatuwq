# Script para criar novo wq_demo a partir de wq_demo_selected.csv
# CSV com 20 linhas selecionadas (5 de cada ponto: FBS-BRH-250, FBS-BRH-300, FBS-BRH-450, FBS-BRH-950)

library(readr)
library(dplyr)
library(tibble)

# Ler CSV selecionado (já tem as 20 linhas selecionadas)
wq_demo <- read_csv("wq_demo_selected.csv", 
                    locale = locale(encoding = "UTF-8"),
                    show_col_types = FALSE)

# Garantir que data seja Date
wq_demo$data <- as.Date(wq_demo$data)

# Ordenar por ponto e data
wq_demo <- wq_demo %>% arrange(ponto, data)

# Converter para tibble
wq_demo <- as_tibble(wq_demo)

# Verificar estrutura
cat("=== Estrutura do novo wq_demo ===\n")
cat("Linhas finais:", nrow(wq_demo), "\n")
cat("Colunas:", ncol(wq_demo), "\n")
cat("\nPontos e linhas por ponto:\n")
print(table(wq_demo$ponto))

# Verificar colunas obrigatórias
required_cols <- c("ponto", "data", "ph", "od", "turbidez", "dbo",
                   "coliformes", "p_total", "nt_total", "temperatura")
missing_cols <- setdiff(required_cols, names(wq_demo))
if (length(missing_cols) > 0) {
  cat("\nAVISO: Colunas faltando:", paste(missing_cols, collapse = ", "), "\n")
} else {
  cat("\nTodas as colunas obrigatórias presentes!\n")
}

# Verificar tds (opcional)
if ("tds" %in% names(wq_demo)) {
  cat("Coluna tds presente (opcional)\n")
}

# Salvar como RDA
save(wq_demo, file = "data/wq_demo.rda", compress = "xz")

cat("\n=== wq_demo.rda criado com sucesso! ===\n")
cat("\nPrimeiras linhas:\n")
print(head(wq_demo))

cat("\nDatas por ponto:\n")
print(wq_demo %>% group_by(ponto) %>% summarise(min_data = min(data), max_data = max(data), n = n()))


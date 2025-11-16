# Script para criar novo wq_demo a partir de dataset-real.csv
# Manualmente seleciona 5 linhas de cada ponto (FBS-BRH-250, FBS-BRH-300, FBS-BRH-450, FBS-BRH-950)

library(readr)
library(dplyr)
library(tibble)

# Ler dataset-real.csv
df <- read_csv("dataset-real.csv", 
               locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE)

# Remover FBS-BRA-900
df <- df %>% filter(ponto != "FBS-BRA-900")

# Remover linhas duplicadas (mesmo ponto + data)
df <- df %>% distinct(ponto, data, .keep_all = TRUE)

# Função para selecionar 5 linhas distribuídas no tempo
select_5_representative <- function(df_ponto) {
  # Ordenar por data
  df_ponto <- df_ponto %>% arrange(data)
  n <- nrow(df_ponto)
  
  # Se tiver <= 5 linhas, retornar todas
  if (n <= 5) {
    return(df_ponto)
  }
  
  # Selecionar índices distribuídos (primeira, última e 3 intermediárias)
  idx <- c(1, round(n*0.25), round(n*0.5), round(n*0.75), n)
  df_ponto[idx, ]
}

# Selecionar 5 linhas de cada ponto
wq_demo_250 <- df %>% filter(ponto == "FBS-BRH-250") %>% select_5_representative()
wq_demo_300 <- df %>% filter(ponto == "FBS-BRH-300") %>% select_5_representative()
wq_demo_450 <- df %>% filter(ponto == "FBS-BRH-450") %>% select_5_representative()
wq_demo_950 <- df %>% filter(ponto == "FBS-BRH-950") %>% select_5_representative()

# Combinar
wq_demo <- bind_rows(wq_demo_250, wq_demo_300, wq_demo_450, wq_demo_950)

# Garantir que data seja Date
wq_demo$data <- as.Date(wq_demo$data)

# Ordenar por ponto e data
wq_demo <- wq_demo %>% arrange(ponto, data)

# Manter apenas colunas necessárias (opcional: manter rio, lat, lon como extras)
# Para manter compatibilidade, vou manter todas as colunas
# wq_demo <- wq_demo %>% select(-rio)

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


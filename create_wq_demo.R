# Script para criar novo wq_demo a partir de dataset-real.csv
library(readr)
library(dplyr)
library(tibble)
library(lubridate)

# Ler dataset-real.csv
df <- read_csv("dataset-real.csv", 
               locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE)

# Remover FBS-BRA-900
df <- df %>% filter(ponto != "FBS-BRA-900")

# Remover linhas duplicadas (mesmo ponto + data)
df <- df %>% distinct(ponto, data, .keep_all = TRUE)

# Função para selecionar 5 linhas representativas de um ponto
select_representative <- function(df_ponto, n = 5) {
  # Ordenar por data
  df_ponto <- df_ponto %>% arrange(data)
  
  # Se tiver <= n linhas, retornar todas
  if (nrow(df_ponto) <= n) {
    return(df_ponto)
  }
  
  # Selecionar distribuindo ao longo do tempo
  idx <- round(seq(1, nrow(df_ponto), length.out = n))
  df_ponto[idx, ]
}

# Selecionar 5 linhas de cada ponto
wq_demo <- bind_rows(
  df %>% filter(ponto == "FBS-BRH-250") %>% select_representative(5),
  df %>% filter(ponto == "FBS-BRH-300") %>% select_representative(5),
  df %>% filter(ponto == "FBS-BRH-450") %>% select_representative(5),
  df %>% filter(ponto == "FBS-BRH-950") %>% select_representative(5)
)

# Garantir que data seja Date
wq_demo$data <- as.Date(wq_demo$data)

# Remover coluna rio (não está no wq_demo original)
# Mas manter lat e lon como opcionais (podem ser úteis)
# wq_demo <- wq_demo %>% select(-rio)

# Ordenar por ponto e data
wq_demo <- wq_demo %>% arrange(ponto, data)

# Verificar estrutura
cat("Linhas finais:", nrow(wq_demo), "\n")
cat("Colunas:", ncol(wq_demo), "\n")
cat("Pontos únicos:", unique(wq_demo$ponto), "\n")
cat("Linhas por ponto:\n")
print(table(wq_demo$ponto))

# Verificar colunas obrigatórias
required_cols <- c("ponto", "data", "ph", "od", "turbidez", "dbo",
                   "coliformes", "p_total", "nt_total", "temperatura")
missing_cols <- setdiff(required_cols, names(wq_demo))
if (length(missing_cols) > 0) {
  cat("AVISO: Colunas faltando:", paste(missing_cols, collapse = ", "), "\n")
} else {
  cat("Todas as colunas obrigatórias presentes!\n")
}

# Salvar como RDA
wq_demo <- as_tibble(wq_demo)
save(wq_demo, file = "data/wq_demo.rda", compress = "xz")

cat("\nwq_demo.rda criado com sucesso!\n")
cat("Primeiras linhas:\n")
print(head(wq_demo))


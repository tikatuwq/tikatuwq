# audit/regressao_cetesb_pos_igam.R
# Regressao do motor CETESB apos a implementacao experimental do IGAM: recalcula
# as 131 linhas de inst/audit/REGRESSAO_CETESB_POS_API.csv e exige diferenca
# zero no IQA bruto, no IQA exibido e na classe.
#
#   Rscript audit/regressao_cetesb_pos_igam.R

suppressMessages(devtools::load_all(".", quiet = TRUE))
b <- read.csv("inst/audit/REGRESSAO_CETESB_POS_API.csv", stringsAsFactors = FALSE)
inp <- b[, grep("^in_", names(b))]
names(inp) <- sub("^in_", "", names(inp))
# As linhas de fronteira do fosforo foram geradas como PO4/3.066 (PO4 = 1, 5,
# 10) e gravadas no CSV com 15 digitos; relidas, caem 1 ulp acima da fronteira
# (ex.: 1.0000000000000007) e mudam de trecho. Reconstroi-se o valor exato.
po4 <- inp$p_total * 3.066
fronteira_p <- !is.na(po4) & abs(po4 - round(po4)) < 1e-9 & round(po4) %in% c(1, 5, 10)
inp$p_total[fronteira_p] <- round(po4[fronteira_p]) / 3.066

calc <- function(i) {
  o <- suppressWarnings(iqa(inp[i, , drop = FALSE], allow_partial = b$allow_partial[i], details = TRUE))
  c(bruto = attr(o, "components")$IQA_unrounded, exibido = o$IQA, classe = as.character(o$IQA_status))
}
r <- t(vapply(seq_len(nrow(b)), calc, character(3)))
out <- data.frame(
  dataset = b$dataset, case = b$case,
  IQA_bruto_ref = b$IQA_bruto_v0110, IQA_bruto_agora = as.numeric(r[, "bruto"]),
  IQA_exibido_ref = b$IQA_exibido_v0110, IQA_exibido_agora = as.numeric(r[, "exibido"]),
  classe_ref = b$classe_v0110, classe_agora = r[, "classe"],
  entrada_reconstruida = ifelse(fronteira_p, "p_total = PO4/3.066 exato (CSV com 15 digitos)", ""),
  stringsAsFactors = FALSE
)
# A referencia guarda o bruto com 4 casas
out$dif_bruto <- round(out$IQA_bruto_agora, 4) - out$IQA_bruto_ref
out$dif_exibido <- out$IQA_exibido_agora - out$IQA_exibido_ref
out$classe_igual <- (is.na(out$classe_ref) & is.na(out$classe_agora)) | out$classe_ref %in% out$classe_agora &
  out$classe_ref == out$classe_agora
utils::write.csv(out, "audit/resultados/regressao_cetesb_pos_igam.csv", row.names = FALSE)
cat("linhas:", nrow(out),
    "| max |dif bruto|:", max(abs(out$dif_bruto), na.rm = TRUE),
    "| max |dif exibido|:", max(abs(out$dif_exibido), na.rm = TRUE),
    "| NA coincidentes:", all(is.na(out$IQA_bruto_ref) == is.na(out$IQA_bruto_agora)),
    "| classes diferentes:", sum(!out$classe_igual, na.rm = TRUE), "\n")

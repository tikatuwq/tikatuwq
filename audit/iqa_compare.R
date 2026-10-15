# audit/iqa_compare.R
# Compara dois snapshots de audit/iqa_snapshot.R (v0.10.0 x atual) e grava
# inst/audit/REGRESSAO_0_10_0_0_11_0.csv. Nao altera nenhum gabarito de teste.
#
#   Rscript audit/iqa_compare.R old.csv new.csv <dir_saida>

args <- commandArgs(trailingOnly = TRUE)
old <- utils::read.csv(args[1], stringsAsFactors = FALSE)
new <- utils::read.csv(args[2], stringsAsFactors = FALSE)
out_dir <- args[3]

stopifnot(nrow(old) == nrow(new), identical(old$case, new$case))

qi_cols <- grep("^qi_", names(new), value = TRUE)
tol <- 1e-6
changed <- vapply(qi_cols, function(k) {
  a <- old[[k]]; b <- new[[k]]
  !(is.na(a) & is.na(b)) & (xor(is.na(a), is.na(b)) | abs(a - b) > tol)
}, logical(nrow(new)))
if (is.null(dim(changed))) changed <- matrix(changed, nrow = 1)

# Justificativa por componente alterado
just <- c(
  qi_dbo = "EQ-01 DBO 5-15: log10 -> ln (curva q3 da Figura 1, Apendice D CETESB 2023)",
  qi_nt  = "EQ-04 NT 10-60: log10 -> ln (curva q4 da Figura 1)",
  qi_ph  = "EQ-02/03 pH 8.5-10: 10^ -> exp (curva q2); 50-32(pH-9) -> 288-27pH (CPRH, pendente)",
  qi_od  = "OD: Cs com 0.00005896 (CPRH; von Sperling via IBEAS); antes 0.00005898"
)
inp <- grep("^in_", names(new), value = TRUE)
cmp <- data.frame(
  new[, c("dataset", "case", "allow_partial", inp)],
  IQA_bruto_v0100 = round(old$IQA_bruto, 4), IQA_bruto_v0110 = round(new$IQA_bruto, 4),
  IQA_exibido_v0100 = old$IQA_exibido, IQA_exibido_v0110 = new$IQA_exibido,
  delta_IQA = round(new$IQA_bruto - old$IQA_bruto, 4),
  classe_v0100 = old$classe_versao, classe_v0110 = new$classe_versao,
  classe_mudou = !mapply(identical, old$classe_versao, new$classe_versao),
  qi_alterados = apply(changed, 1, function(r) paste(sub("^qi_", "", qi_cols[r]), collapse = ";")),
  justificativa = apply(changed, 1, function(r) paste(just[qi_cols[r]][!is.na(just[qi_cols[r]])], collapse = " | ")),
  fonte = "CETESB (2023) Apendice D, Figura 1; CPRH PNMA II; von Sperling (2007) via IBEAS (2015)",
  stringsAsFactors = FALSE
)
for (k in qi_cols) {
  cmp[[paste0(k, "_v0100")]] <- round(old[[k]], 4)
  cmp[[paste0(k, "_v0110")]] <- round(new[[k]], 4)
}
cmp$fonte[cmp$qi_alterados == ""] <- ""

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
utils::write.csv(cmp, file.path(out_dir, "REGRESSAO_0_10_0_0_11_0.csv"), row.names = FALSE)

summ <- do.call(rbind, lapply(split(cmp, cmp$dataset), function(x) {
  data.frame(
    dataset = x$dataset[1], n = nrow(x),
    n_IQA_alterado = sum(abs(x$delta_IQA) > 0.005, na.rm = TRUE),
    max_abs_delta = round(max(abs(x$delta_IQA), na.rm = TRUE), 2),
    n_classe_alterada = sum(x$classe_mudou)
  )
}))
print(summ, row.names = FALSE)
cat("\nLinhas com |delta| > 0.005 ou classe alterada:\n")
ch <- cmp[abs(cmp$delta_IQA) > 0.005 | cmp$classe_mudou,
          c("dataset", "case", "IQA_bruto_v0100", "IQA_bruto_v0110", "delta_IQA", "classe_v0100", "classe_v0110", "qi_alterados")]
print(ch, row.names = FALSE)

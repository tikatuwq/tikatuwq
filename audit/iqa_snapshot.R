# audit/iqa_snapshot.R
# Exporta entradas, qi, IQA bruto, IQA exibido e classes de um conjunto fixo de
# dados para uma versao do pacote. Rodar uma vez por versao e comparar com
# audit/iqa_compare.R:
#
#   git worktree add --detach ../tikatuwq_old 4e88fa9
#   Rscript audit/iqa_snapshot.R ../tikatuwq_old old.csv
#   Rscript audit/iqa_snapshot.R .               new.csv
#   Rscript audit/iqa_compare.R  old.csv new.csv inst/audit
#
# O dataset sintetico e fixo e cobre as fronteiras dos trechos das equacoes e
# as fronteiras de classe (19/36/51/79). O IQA bruto e recalculado a partir dos
# qi e dos pesos CETESB (renormalizados sobre os qi disponiveis quando
# allow_partial = TRUE), de forma identica para qualquer versao.

args <- commandArgs(trailingOnly = TRUE)
pkg_path <- args[1]
out_path <- args[2]

suppressMessages(devtools::load_all(pkg_path, quiet = TRUE, export_all = TRUE))

base_row <- list(
  od = 7, temperatura = 25, ph = 7.5, dbo = 2, coliformes = 50,
  nt_total = 2, p_total = 0.05, turbidez = 10, solidos_totais = 100
)
eps <- 1e-3
sweep <- list(
  dbo      = c(0, 2, 5 - eps, 5, 5 + eps, 8, 10, 12, 15 - eps, 15, 15 + eps, 20, 25, 30, 30 + eps, 40),
  ph       = c(2, 4, 6.2, 7, 8, 8.5 - eps, 8.5, 8.5 + eps, 8.6, 8.8, 9 - eps, 9, 9 + eps, 9.5, 10, 10 + eps, 11, 12, 12.5),
  nt_total = c(0.5, 5, 10, 10 + eps, 20, 30, 40, 60, 60 + eps, 80, 100, 100 + eps, 120),
  od       = 8.263 * c(0.2, 0.5, 0.85, 1.0, 1.2, 1.4, 1.41, 1.6),
  p_total  = c(0.01, 0.1, 1 / 3.066, 0.5, 5 / 3.066, 3, 10 / 3.066, 5),
  turbidez = c(1, 25, 25 + eps, 60, 100, 100 + eps, 150),
  solidos_totais = c(20, 150, 150 + eps, 300, 500, 500 + eps, 800),
  coliformes = c(0, 1, 10, 100, 1e4, 1e5, 1e5 + 1, 1e6)
)
rows <- list()
for (p in names(sweep)) {
  for (v in sweep[[p]]) {
    r <- base_row
    r[[p]] <- v
    rows[[length(rows) + 1L]] <- c(r, list(dataset = "grade_fronteiras", case = paste0(p, "=", signif(v, 7))))
  }
}
# Amostras perto das fronteiras de classe: piora gradual de coliformes e DBO
for (k in seq_len(25)) {
  r <- base_row
  r$coliformes <- round(10^(1 + 0.16 * k))
  r$dbo <- 1 + 0.45 * k
  r$od <- 7.2 - 0.18 * k
  rows[[length(rows) + 1L]] <- c(r, list(dataset = "gradiente_classes", case = paste0("k=", k)))
}
grid <- do.call(rbind, lapply(rows, as.data.frame))

data(wq_demo, package = "tikatuwq", envir = environment())
demo <- as.data.frame(wq_demo)
demo$dataset <- "wq_demo"
demo$case <- paste(demo$ponto, as.character(demo$data))

w_cetesb <- c(od = 0.17, colif = 0.15, ph = 0.12, dbo = 0.10, nt = 0.10,
              pt = 0.10, temp = 0.10, turb = 0.08, st = 0.08)

run <- function(d, partial) {
  o <- suppressWarnings(iqa(d, allow_partial = partial, details = TRUE))
  cp <- attr(o, "components")
  qi <- as.matrix(cp[, paste0("qi_", names(w_cetesb))])
  raw <- apply(qi, 1, function(q) {
    ok <- is.finite(q)
    if (!any(ok) || (!partial && !all(ok))) return(NA_real_)
    ww <- w_cetesb[ok] / sum(w_cetesb[ok])
    prod(q[ok]^ww)
  })
  inp <- c("od", "temperatura", "ph", "dbo", "coliformes", "nt_total", "p_total", "turbidez", "solidos_totais")
  inputs <- as.data.frame(lapply(inp, function(k) if (k %in% names(d)) as.character(d[[k]]) else NA_character_))
  names(inputs) <- paste0("in_", inp)
  data.frame(
    dataset = d$dataset, case = d$case, allow_partial = partial,
    inputs,
    cp[, grep("^qi_", names(cp))],
    IQA_bruto = raw,
    IQA_exibido = o$IQA,
    classe_versao = as.character(o$IQA_status),
    classe_cetesb_bruto = as.character(cut(raw, c(-Inf, 19, 36, 51, 79, Inf),
                                           c("Pessima", "Ruim", "Regular", "Boa", "Otima"))),
    stringsAsFactors = FALSE
  )
}
res <- rbind(run(grid, FALSE), run(demo, TRUE))
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, out_path, row.names = FALSE)
cat("Wrote", nrow(res), "rows to", out_path, "\n")

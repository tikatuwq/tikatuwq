# audit/iqa_sensitivity.R
# Sensibilidade do IQA as escolhas em disputa entre as fontes:
#   A  atual (ln em DBO/NT; pH 9-10 = 288 - 27 pH; OD > 140% = 47)
#   B  transcricao de von Sperling (2007) via IBEAS (2015): log10 em DBO/NT,
#      pH 9-10 = 50 - 32 (pH - 9), OD > 140% = 50
#   C  so pH 9-10 = 50 - 32 (pH - 9)
#   D  so OD > 140% = 50 (CPRH e IBEAS: "47 + 3")
# Saida: inst/audit/sensibilidade_variantes.csv
#
#   Rscript audit/iqa_sensitivity.R

suppressMessages(devtools::load_all(".", quiet = TRUE, export_all = TRUE))
data(wq_demo, package = "tikatuwq", envir = environment())

base <- data.frame(od = 7, temperatura = 25, ph = 7.5, dbo = 2, coliformes = 50,
                   nt_total = 2, p_total = 0.05, turbidez = 10, solidos_totais = 100)
mk <- function(col, vals) { d <- base[rep(1, length(vals)), ]; d[[col]] <- vals; d$case <- paste0(col, "=", vals); d }
syn <- rbind(mk("dbo", c(6, 8, 10, 12, 14)), mk("nt_total", c(12, 20, 30, 45, 58)),
             mk("ph", c(9.1, 9.3, 9.5, 9.8)), mk("od", 8.263 * c(1.45, 1.6, 1.8)))
syn$dataset <- "sintetico"
demo <- as.data.frame(wq_demo); demo$case <- paste(demo$ponto, demo$data); demo$dataset <- "wq_demo"

w <- c(od = 0.17, colif = 0.15, ph = 0.12, dbo = 0.10, nt = 0.10, pt = 0.10, temp = 0.10, turb = 0.08, st = 0.08)
agg <- function(qi) apply(qi, 1, function(q) { ok <- is.finite(q); if (!any(ok)) return(NA); prod(q[ok]^(w[ok] / sum(w[ok]))) })

variants <- function(d) {
  cp <- attr(suppressWarnings(iqa(d, allow_partial = TRUE, details = TRUE)), "components")
  qi <- as.matrix(cp[, paste0("qi_", names(w))]); colnames(qi) <- names(w)
  dbo <- cp$raw_dbo; nt <- cp$raw_nt; ph <- cp$raw_ph
  cs <- (14.62 - 0.3898 * cp$raw_temp + 0.006969 * cp$raw_temp^2 - 0.00005896 * cp$raw_temp^3)
  psat <- 100 * cp$raw_od / cs
  b_dbo <- ifelse(!is.na(dbo) & dbo > 5 & dbo <= 15, 104.67 - 31.5463 * log10(dbo), qi[, "dbo"])
  b_nt  <- ifelse(!is.na(nt) & nt > 10 & nt <= 60, 101.9 - 23.1023 * log10(nt), qi[, "nt"])
  c_ph  <- ifelse(!is.na(ph) & ph > 9 & ph <= 10, 50 - 32 * (ph - 9), qi[, "ph"])
  d_od  <- ifelse(!is.na(psat) & psat > 140, 50, qi[, "od"])
  qB <- qi; qB[, "dbo"] <- b_dbo; qB[, "nt"] <- b_nt; qB[, "ph"] <- c_ph; qB[, "od"] <- d_od
  qC <- qi; qC[, "ph"] <- c_ph
  qD <- qi; qD[, "od"] <- d_od
  cls <- function(x) as.character(classify_iqa(pmin(100, pmax(0, x))))
  A <- agg(qi); B <- agg(qB); C <- agg(qC); D <- agg(qD)
  data.frame(dataset = d$dataset, case = d$case,
             IQA_A = round(A, 2), IQA_B = round(B, 2), IQA_C = round(C, 2), IQA_D = round(D, 2),
             dB = round(B - A, 2), dC = round(C - A, 2), dD = round(D - A, 2),
             classe_A = cls(A), classe_B = cls(B), classe_C = cls(C), classe_D = cls(D))
}
res <- rbind(variants(syn), variants(demo))
utils::write.csv(res, "inst/audit/sensibilidade_variantes.csv", row.names = FALSE)
print(res[res$dataset == "sintetico" | res$dB != 0 | res$dC != 0 | res$dD != 0, ], row.names = FALSE)
s <- function(x) c(max_abs = max(abs(x)), n_changed = sum(x != 0))
cat("\nwq_demo: B", s(res$dB[res$dataset == "wq_demo"]), " C", s(res$dC[res$dataset == "wq_demo"]),
    " D", s(res$dD[res$dataset == "wq_demo"]), "\n")
cat("Classe diferente de A (todas as linhas): B", sum(res$classe_B != res$classe_A),
    " C", sum(res$classe_C != res$classe_A), " D", sum(res$classe_D != res$classe_A), "\n")

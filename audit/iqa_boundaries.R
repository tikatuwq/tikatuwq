# audit/iqa_boundaries.R
# Avalia cada qi em b - eps, b e b + eps (eps = 1e-6) em todas as fronteiras de
# trecho, para a versao atual e para as formulas da v0.10.0 nos trechos que
# mudaram (DBO 5-15, NT 10-60, pH 8.5-10, Cs do OD). Tambem avalia valores
# ambientais representativos. Saida: inst/audit/fronteiras_qi.csv
#
#   Rscript audit/iqa_boundaries.R

suppressMessages(devtools::load_all(".", quiet = TRUE, export_all = TRUE))
eps <- 1e-6

# Formulas da v0.10.0 (commit 4e88fa9) nos trechos alterados
old_dbo <- function(C) ifelse(C > 5 & C <= 15, 104.67 - 31.5463 * log10(C), .qi_cetesb_dbo(C))
old_nt  <- function(C) ifelse(C > 10 & C <= 60, 101.9 - 23.1023 * log10(C), .qi_cetesb_nt(C))
old_ph  <- function(p) {
  out <- .qi_cetesb_ph(p)
  i <- p > 8.5 & p <= 9; out[i] <- 1415823 * 10^(-1.1507 * p[i])
  i <- p > 9 & p <= 10;  out[i] <- 50 - 32 * (p[i] - 9)
  pmin(100, pmax(0, out))
}
psat_od <- function(p, k) {
  cs <- (14.62 - 0.3898 * 25 + 0.006969 * 25^2 - k * 25^3)
  p / 100 * cs
}

spec <- list(
  dbo        = list(new = .qi_cetesb_dbo, old = old_dbo, b = c(0, 5, 15, 30),
                    typ = c(2, 5, 10, 15, 20, 30, 35), unit = "mg/L"),
  nt_total   = list(new = .qi_cetesb_nt, old = old_nt, b = c(10, 60, 100),
                    typ = c(0.5, 1, 2, 5, 10, 20, 40, 60, 80, 100, 120), unit = "mg/L N"),
  ph         = list(new = .qi_cetesb_ph, old = old_ph, b = c(2, 4, 6.2, 7, 8, 8.5, 9, 10, 12),
                    typ = c(5, 6, 6.5, 7, 7.5, 8, 8.6, 9, 9.5, 10, 11), unit = "pH"),
  p_total_PO4 = list(new = function(x) .qi_cetesb_pt(x, "PO4"), old = NULL, b = c(1, 5, 10),
                    typ = c(0.03, 0.1, 0.3, 1, 3, 10), unit = "mg/L PO4"),
  turbidez   = list(new = .qi_cetesb_turbidez, old = NULL, b = c(25, 100),
                    typ = c(1, 5, 25, 50, 100, 150), unit = "UNT"),
  solidos_totais = list(new = .qi_cetesb_solidos_totais, old = NULL, b = c(150, 500),
                    typ = c(50, 100, 150, 300, 500, 600), unit = "mg/L"),
  coliformes = list(new = .qi_cetesb_coliformes, old = NULL, b = c(1, 10, 1e5),
                    typ = c(1, 10, 100, 1e3, 1e4, 1e5, 1e6), unit = "NMP/100 mL"),
  od_psat    = list(new = function(p) .qi_cetesb_od(psat_od(p, 5.896e-5), 25, 0),
                    # v0.10.0 usava Cs com 5.898e-5: para o mesmo OD em mg/L, a
                    # %sat antiga e OD / Cs_old; emulada reescalando o OD.
                    old = function(p) {
                      cs_old <- 14.62 - 0.3898 * 25 + 0.006969 * 25^2 - 5.898e-5 * 25^3
                      cs_new <- 14.62 - 0.3898 * 25 + 0.006969 * 25^2 - 5.896e-5 * 25^3
                      .qi_cetesb_od(psat_od(p, 5.896e-5) * cs_new / cs_old, 25, 0)
                    },
                    b = c(50, 85, 100, 140), typ = c(20, 50, 70, 85, 100, 120, 140, 160), unit = "% sat (25 C, 0 m)"),
  delta_t    = list(new = function(x) suppressWarnings(.qi_cetesb_temperatura(x, "delta", n = length(x))),
                    old = NULL, b = c(-5, 0, 5, 15), typ = c(-5, 0, 5, 10, 15), unit = "C")
)

rows <- list()
for (nm in names(spec)) {
  s <- spec[[nm]]
  for (b in s$b) {
    x <- c(b - eps, b, b + eps)
    qn <- s$new(x)
    qo <- if (is.null(s$old)) rep(NA_real_, 3) else s$old(x)
    rows[[length(rows) + 1L]] <- data.frame(
      parametro = nm, unidade = s$unit, tipo = "fronteira", x = b,
      qi_esq_atual = qn[1], qi_atual = qn[2], qi_dir_atual = qn[3],
      salto_atual = qn[3] - qn[2],
      qi_esq_v0100 = qo[1], qi_v0100 = qo[2], qi_dir_v0100 = qo[3],
      salto_v0100 = qo[3] - qo[2]
    )
  }
  qn <- s$new(s$typ)
  qo <- if (is.null(s$old)) rep(NA_real_, length(s$typ)) else s$old(s$typ)
  rows[[length(rows) + 1L]] <- data.frame(
    parametro = nm, unidade = s$unit, tipo = "valor_tipico", x = s$typ,
    qi_esq_atual = NA, qi_atual = qn, qi_dir_atual = NA, salto_atual = NA,
    qi_esq_v0100 = NA, qi_v0100 = qo, qi_dir_v0100 = NA, salto_v0100 = NA
  )
}
res <- do.call(rbind, rows)
num <- vapply(res, is.numeric, logical(1))
res[num] <- lapply(res[num], function(v) round(v, 6))
utils::write.csv(res, "inst/audit/fronteiras_qi.csv", row.names = FALSE)
print(res[res$parametro %in% c("dbo", "nt_total", "ph"), ], row.names = FALSE)

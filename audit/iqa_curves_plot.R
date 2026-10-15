# audit/iqa_curves_plot.R
# Desenha as curvas qi do pacote nos mesmos eixos da Figura 1 do Apendice D
# CETESB 2023 (p. 4), para comparacao visual lado a lado com a fonte.
#
#   Rscript audit/iqa_curves_plot.R inst/audit/curvas_qi_pacote.png

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else "inst/audit/curvas_qi_pacote.png"
suppressMessages(devtools::load_all(".", quiet = TRUE, export_all = TRUE))

panel <- function(x, y, main, xlab, log = "", note = NULL, xlim = range(x)) {
  plot(x, y, type = "l", lwd = 2, ylim = c(0, 100), xlim = xlim, log = log,
       main = main, xlab = xlab, ylab = "qi", las = 1, cex.main = 0.95)
  abline(h = seq(0, 100, 10), col = "grey85", lty = 3)
  if (!is.null(note)) mtext(note, side = 3, line = 0.1, cex = 0.6, col = "grey30")
}

grDevices::png(out, width = 1500, height = 1500, res = 140)
op <- par(mfrow = c(3, 3), mar = c(4, 4, 2.5, 1))

x <- 10^seq(0, 5, length.out = 400)
panel(x, .qi_cetesb_coliformes(x), "Coliformes termotolerantes (w = 0,15)",
      "C.T. UFC/100 mL", log = "x")
x <- seq(2, 12, length.out = 1001)
panel(x, .qi_cetesb_ph(x), "pH (w = 0,12)", "pH",
      note = "EQ-02 (8,5-9) e EQ-03 (9-10)")
abline(v = c(8.5, 9, 10), col = "red", lty = 2)
x <- seq(0, 30, length.out = 601)
panel(x, .qi_cetesb_dbo(x), "DBO5 (w = 0,10)", "DBO5 mg/L", xlim = c(0, 50),
      note = "EQ-01 (5-15)")
x <- seq(0, 100, length.out = 501)
panel(x, .qi_cetesb_nt(x), "Nitrogenio total (w = 0,10)", "N.T. mg/L",
      note = "EQ-04 (10-60)")
x <- seq(0, 10, length.out = 501)
panel(x, .qi_cetesb_pt(x, "PO4"), "Fosforo total (w = 0,10)", "PO4 mg/L")
x <- seq(-5, 15, length.out = 401)
panel(x, suppressWarnings(.qi_cetesb_temperatura(x, "delta", n = length(x))),
      "Temperatura (w = 0,10)", "Delta t, C", xlim = c(-5, 20),
      note = "EQ-07/08: diverge da Figura 1")
x <- seq(0, 100, length.out = 501)
panel(x, .qi_cetesb_turbidez(x), "Turbidez (w = 0,08)", "U.F.T.")
x <- seq(0, 500, length.out = 501)
panel(x, .qi_cetesb_solidos_totais(x), "Solidos totais (w = 0,08)", "S.T. mg/L")
psat <- seq(0, 140, length.out = 701)
panel(psat, .qi_cetesb_od(psat / 100 * 8.2626, temp = 25, altitude_m = 0),
      "Oxigenio dissolvido (w = 0,17)", "O.D. % de saturacao", xlim = c(0, 200))

par(op)
invisible(grDevices::dev.off())
cat("Wrote", out, "\n")

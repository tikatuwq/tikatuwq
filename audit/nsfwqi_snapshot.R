# audit/nsfwqi_snapshot.R
# Exporta NSFWQI (duas agregacoes) para o wq_demo e uma grade fixa, para uma
# versao do pacote. Uso: Rscript audit/nsfwqi_snapshot.R <pkg_path> <out.csv>

args <- commandArgs(trailingOnly = TRUE)
suppressMessages(devtools::load_all(args[1], quiet = TRUE))
data(wq_demo, package = "tikatuwq", envir = environment())

grid <- data.frame(
  od = c(4, 6.5, 8, 6.5, 6.5), coliformes = c(5000, 200, 10, 200, 200),
  ph = c(6, 7.2, 7.5, 7.2, 7.2), dbo = c(8, 2, 1, 2, 2), temperatura = 25,
  p_ortofosfato = c(0.3, 0.05, 0.01, 0.05, 0.05), n_nitrato = c(12, 1, 0.2, 1, 1),
  turbidez = c(80, 8, 2, 8, 8), solidos_totais = c(600, 120, 50, NA, 120)
)
grid$solidos_suspensos <- c(NA, NA, NA, 40, NA)   # linha 4: so TSS
grid$case <- paste0("grade_", seq_len(nrow(grid)))
demo <- as.data.frame(wq_demo); demo$case <- paste(demo$ponto, demo$data)

run <- function(d, ds) {
  g <- suppressWarnings(nsfwqi(d, na_rm = TRUE))
  a <- suppressWarnings(nsfwqi(d, na_rm = TRUE, aggregation = "brown1970_arithmetic"))
  s <- suppressWarnings(nsfwqi(d, na_rm = FALSE))
  data.frame(dataset = ds, case = d$case, NSFWQI_geo_parcial = g$NSFWQI,
             NSFWQI_arit_parcial = a$NSFWQI, NSFWQI_geo_estrito = s$NSFWQI,
             status_geo = g$NSFWQI_status)
}
res <- rbind(run(grid, "grade"), run(demo, "wq_demo"))
utils::write.csv(res, args[2], row.names = FALSE)
cat("Wrote", nrow(res), "rows\n")

# audit/iqa_boundaries.R
# Avalia cada qi em b - eps, b e b + eps (eps = 1e-6) em todas as fronteiras de
# trecho, para a versao atual e para as formulas da v0.10.0 nos trechos que
# mudaram (DBO 5-15, NT 10-60, pH 8.5-10, Cs do OD). Tambem avalia valores
# ambientais representativos. Saidas: inst/audit/FRONTEIRAS_QI.csv e audit/resultados/fronteiras_qi_completo.csv
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
utils::write.csv(res, "audit/resultados/fronteiras_qi_completo.csv", row.names = FALSE)
invisible(NULL)

# ---------------------------------------------------------------------------
# FRONTEIRAS_QI.csv: formato da Etapa 1 (parametro, limite, epsilon, qi
# esquerda/limite/direita, fonte, conclusao, acao), mantendo as colunas da
# v0.10.0. Classificacao do salto:
#   a = erro de codigo/transcricao (corrigido)
#   b = comportamento da referencia (valor fixo oficial ou artefato do ajuste
#       publicado nas duas transcricoes)
#   c = indeterminado
# ---------------------------------------------------------------------------
ann <- read.csv(text = '
parametro,x,classe_salto,fonte,conclusao,acao
dbo,0,b,CPRH; von Sperling/IBEAS,continuo,manter
dbo,5,b,CPRH; von Sperling/IBEAS; curva CETESB q3,"v0.10.0: salto +28.65 por log10 (erro a); atual -0.07 (ajuste)",corrigido na 0.11.0
dbo,15,b,CPRH; von Sperling/IBEAS,"+0.39: ramos ajustados separadamente; coeficientes identicos nas duas transcricoes",manter; documentado
dbo,30,b,CETESB Figura 1 (nota DBO > 30 -> 2),valor fixo oficial,manter
nt_total,10,b,CPRH; von Sperling/IBEAS; curva CETESB q4,"v0.10.0: +29.9 por log10 (erro a); atual -0.20",corrigido na 0.11.0
nt_total,60,b,CPRH; von Sperling/IBEAS,"+0.03: artefato do ajuste",manter; documentado
nt_total,100,b,CETESB Figura 1 (nota NT > 100 -> 1),"+0.056: ajuste termina em 0.944 abaixo do valor fixo",manter; documentado
ph,2,b,CETESB Figura 1 (pH < 2 -> 2),"+0.15",manter
ph,4,b,CPRH; von Sperling/IBEAS,"-0.15",manter
ph,6.2,b,CPRH; von Sperling/IBEAS,"+0.23",manter
ph,7,b,CPRH; von Sperling/IBEAS,"-0.05",manter
ph,8,b,CPRH; von Sperling/IBEAS,"-0.12",manter
ph,8.5,b,CPRH; von Sperling/IBEAS; curva CETESB q2,"v0.10.0: -80 por 10^ (erro a); atual +0.02",corrigido na 0.11.0
ph,9,c,CPRH (228-27pH; tipografico) x von Sperling/IBEAS (50-32(pH-9)),"atual -0.01 (288-27pH); variante IBEAS: +5",decisao do mantenedor
ph,10,b,CPRH; von Sperling/IBEAS,continuo,manter
ph,12,b,CETESB Figura 1 (pH > 12 -> 3),continuo,manter
p_total_PO4,1,b,CPRH; von Sperling/IBEAS,"-0.05",manter
p_total_PO4,5,b,CPRH; von Sperling/IBEAS,"+0.03",manter
p_total_PO4,10,b,CETESB Figura 1 (PO4 > 10 -> 5),"-0.11",manter
turbidez,25,b,CPRH; von Sperling/IBEAS,"-0.49",manter
turbidez,100,b,CETESB Figura 1 (turbidez > 100 -> 5),"-11.8: valor fixo oficial",manter
solidos_totais,150,b,CPRH; von Sperling/IBEAS,"+0.62",manter
solidos_totais,500,b,CETESB Figura 1 (ST > 500 -> 32),"-0.09",manter
coliformes,1,b,CPRH; von Sperling/IBEAS,continuo,manter
coliformes,10,b,CPRH; von Sperling/IBEAS,"-0.59",manter
coliformes,1e+05,b,CETESB Figura 1 (CT > 1e5 -> 3),"-1.19",manter
od_psat,50,b,CPRH; von Sperling/IBEAS,continuo,manter
od_psat,85,b,CPRH; von Sperling/IBEAS,continuo,manter
od_psat,100,b,CPRH; von Sperling/IBEAS,continuo,manter
od_psat,140,b,CETESB Figura 1 (OD > 140% -> 47),"~74 -> 47: valor fixo oficial (secundarias: 50)",manter 47
delta_t,-5,c,CETESB Figura 1 (dT < -5 indefinido); curva do pacote sem fonte,"abaixo de -5 -> NA",modo experimental
delta_t,0,c,curva do pacote sem fonte,"curva diverge da q6 oficial",modo experimental
delta_t,5,c,curva do pacote sem fonte,"+7.0: salto sem fonte",modo experimental; decisao do mantenedor
delta_t,15,b,CETESB Figura 1 (dT > 15 -> 9),"-36.5 (parte do salto vem da curva sem fonte)",modo experimental
', strip.white = TRUE, stringsAsFactors = FALSE)
fb <- res[res$tipo == "fronteira", ]
fr <- data.frame(
  parametro = fb$parametro, unidade = fb$unidade, limite = fb$x, epsilon = eps,
  qi_esquerda = fb$qi_esq_atual, qi_limite = fb$qi_atual, qi_direita = fb$qi_dir_atual,
  salto = fb$salto_atual,
  qi_esquerda_v0100 = fb$qi_esq_v0100, qi_limite_v0100 = fb$qi_v0100,
  qi_direita_v0100 = fb$qi_dir_v0100, salto_v0100 = fb$salto_v0100
)
fr <- merge(fr, ann, by.x = c("parametro", "limite"), by.y = c("parametro", "x"), all.x = TRUE, sort = FALSE)
stopifnot(!anyNA(fr$classe_salto))
utils::write.csv(fr, "inst/audit/FRONTEIRAS_QI.csv", row.names = FALSE)
cat("FRONTEIRAS_QI.csv:", nrow(fr), "fronteiras; classes:", paste(names(table(fr$classe_salto)), table(fr$classe_salto), collapse = " "), "\n")

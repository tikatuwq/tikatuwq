# audit/iqa_matrix.R
# Gera inst/audit/MATRIZ_VALIDACAO_QI.csv: uma linha por trecho de cada qi,
# com equacao, fonte, status de evidencia e os limites laterais no extremo
# superior do trecho (b - eps e b + eps, eps = 1e-6).
#
# Status de evidencia:
#   a_oficial_literal    texto/nota do Apendice D CETESB 2023
#   b_secundaria         equacao rastreada a fonte secundaria (CPRH; von Sperling via IBEAS)
#   b_secundaria_conflito fontes secundarias divergem; escolha guiada pela curva oficial
#   c_derivacao_propria  escolha do pacote sem transcricao direta
#   d_sem_fonte          sem fonte rastreavel; nao validado
#
#   Rscript audit/iqa_matrix.R

suppressMessages(devtools::load_all(".", quiet = TRUE, export_all = TRUE))
eps <- 1e-6
CET <- "CETESB (2023) Apendice D, Figura 1 (p. 4)"
SEC <- "CPRH PNMA II (planilha CETESB); von Sperling (2007, p. 255) via IBEAS (2015)"
od_p <- function(p) .qi_cetesb_od(p / 100 * (14.62 - 0.3898 * 25 + 0.006969 * 625 - 0.00005896 * 15625), 25, 0)
tmp <- function(x) suppressWarnings(.qi_cetesb_temperatura(x, "delta", n = length(x)))
pt4 <- function(x) .qi_cetesb_pt(x, "PO4")

m <- list(
  list("od", "%sat <= 50", "3 + 0.34 s + 0.008095 s^2 + 1.35252e-5 s^3", "% sat", SEC, "b_secundaria", od_p, 50, "concorda com gabarito IBEAS (+-0.15)", "-", "manter"),
  list("od", "50 < %sat <= 85", "3 - 1.166 s + 0.058 s^2 - 3.803435e-4 s^3", "% sat", SEC, "b_secundaria", od_p, 85, "IBEAS 82% -> 87.60 (pacote 87.67)", "-", "manter"),
  list("od", "85 < %sat <= 100", "3 + 3.7745 s^0.704889", "% sat", SEC, "b_secundaria", od_p, 100, "-", "-", "manter"),
  list("od", "100 < %sat <= 140", "3 + 2.9 s - 0.02496 s^2 + 5.60919e-5 s^3", "% sat", SEC, "b_secundaria", od_p, 140, "-", "-", "manter"),
  list("od", "%sat > 140", "47", "% sat", CET, "a_oficial_literal", od_p, NA, "CPRH/IBEAS: 47+3 = 50 (divergem do oficial)", "variante 50: +0.75 IQA", "manter 47 (texto oficial)"),
  list("od", "Cs(T, h)", "(14.62 - 0.3898T + 0.006969T^2 - 0.00005896T^3)(1 - 2.28675e-5 h)^5.167", "mg/L", SEC, "b_secundaria", NULL, NA, "nao consta do Apendice D", "correcao 5.898e-5 -> 5.896e-5: < 0.02% em Cs", "corrigido"),
  list("coliformes", "C <= 1", "100", "NMP/100 mL", SEC, "b_secundaria", .qi_cetesb_coliformes, 1, "-", "-", "manter"),
  list("coliformes", "1 < C <= 10", "100 - 33 log10 C", "NMP/100 mL", SEC, "b_secundaria", .qi_cetesb_coliformes, 10, "IBEAS: 2 -> 90.10; 10 -> 67.00", "-", "manter"),
  list("coliformes", "10 < C <= 1e5", "100 - 37.2 L + 3.60743 L^2 (L = log10 C)", "NMP/100 mL", SEC, "b_secundaria", .qi_cetesb_coliformes, 1e5, "-", "-", "manter"),
  list("coliformes", "C > 1e5", "3", "NMP/100 mL", CET, "a_oficial_literal", .qi_cetesb_coliformes, NA, "IBEAS 160000 -> 3", "-", "manter"),
  list("coliformes", "E. coli", "C x 1.25", "NMP/100 mL", "CETESB (2023) Apendice D, p. 5", "a_oficial_literal", NULL, NA, "-", "-", "manter"),
  list("ph", "pH <= 2", "2", "-", CET, "a_oficial_literal", .qi_cetesb_ph, 2, "-", "-", "manter"),
  list("ph", "2 < pH <= 4", "13.6 - 10.6 pH + 2.4364 pH^2", "-", SEC, "b_secundaria", .qi_cetesb_ph, 4, "-", "-", "manter"),
  list("ph", "4 < pH <= 6.2", "155.5 - 77.36 pH + 10.2481 pH^2", "-", SEC, "b_secundaria", .qi_cetesb_ph, 6.2, "-", "-", "manter"),
  list("ph", "6.2 < pH <= 7", "-657.2 + 197.38 pH - 12.9167 pH^2", "-", SEC, "b_secundaria", .qi_cetesb_ph, 7, "IBEAS 6.8 -> 87.72", "-", "manter"),
  list("ph", "7 < pH <= 8", "-427.8 + 142.05 pH - 9.695 pH^2", "-", SEC, "b_secundaria", .qi_cetesb_ph, 8, "IBEAS 7.1 -> 92.00; 7.3 -> 92.50", "-", "manter"),
  list("ph", "8 < pH <= 8.5", "216 - 16 pH", "-", SEC, "b_secundaria", .qi_cetesb_ph, 8.5, "-", "-", "manter"),
  list("ph", "8.5 < pH <= 9", "1415823 exp(-1.1507 pH)", "-", paste(SEC, "+", CET), "b_secundaria", .qi_cetesb_ph, 9, "v0.10.0 usava 10^: qi ~0.0002 (curva oficial ~70)", "ate +67 IQA (wq_demo)", "corrigido (EQ-02)"),
  list("ph", "9 < pH <= 10", "288 - 27 pH", "-", "CPRH ('228', erro tipografico) x von Sperling/IBEAS (50 - 32(pH - 9))", "b_secundaria_conflito", .qi_cetesb_ph, 10, "fontes secundarias divergem; curva oficial sem salto em 9", "variante IBEAS: ate +0.88 IQA", "decisao do mantenedor (EQ-03)"),
  list("ph", "10 < pH <= 12", "633 - 106.5 pH + 4.5 pH^2", "-", SEC, "b_secundaria", .qi_cetesb_ph, 12, "-", "-", "manter"),
  list("ph", "pH > 12", "3", "-", CET, "a_oficial_literal", .qi_cetesb_ph, NA, "-", "-", "manter"),
  list("dbo", "0 <= C <= 5", "99.96 exp(-0.1232728 C)", "mg/L", SEC, "b_secundaria", .qi_cetesb_dbo, 5, "IBEAS 2 -> 78.12; 3 -> 69.06", "-", "manter"),
  list("dbo", "5 < C <= 15", "104.67 - 31.5463 ln C", "mg/L", paste(SEC, "+", CET), "b_secundaria_conflito", .qi_cetesb_dbo, 15, "CPRH 'log' (ambiguo); IBEAS LOG10 (DBO 12 -> 70.6); curva oficial so compativel com ln (DBO 10 ~30)", "variante log10: ate +8.5 IQA", "corrigido (EQ-01); manter ln"),
  list("dbo", "15 < C <= 30", "4394.91 C^-1.99809", "mg/L", SEC, "b_secundaria", .qi_cetesb_dbo, 30, "IBEAS 20 -> 11.05; salto +0.39 em 15 e do ajuste", "+0.2% IQA em 15-15.15 mg/L", "manter; documentado"),
  list("dbo", "C > 30", "2", "mg/L", CET, "a_oficial_literal", .qi_cetesb_dbo, NA, "-", "-", "manter"),
  list("nt_total", "0 <= C <= 10", "100 - 8.169 C + 0.3059 C^2", "mg/L N", SEC, "b_secundaria", .qi_cetesb_nt, 10, "IBEAS 0.11 -> 99.10; 0.14 -> 98.90; 0.30 -> 97.58", "-", "manter"),
  list("nt_total", "10 < C <= 60", "101.9 - 23.1023 ln C", "mg/L N", paste(SEC, "+", CET), "b_secundaria_conflito", .qi_cetesb_nt, 60, "CPRH 'log'; IBEAS LOG10; curva oficial compativel com ln (NT 20 ~32)", "variante log10: ate +14 IQA", "corrigido (EQ-04); manter ln"),
  list("nt_total", "60 < C <= 100", "159.3148 exp(-0.0512842 C)", "mg/L N", SEC, "b_secundaria", .qi_cetesb_nt, 100, "CPRH escreve 2.7828 (erro por e); salto +0.03 em 60 e do ajuste", "< 0.1% IQA", "manter; documentado"),
  list("nt_total", "C > 100", "1", "mg/L N", CET, "a_oficial_literal", .qi_cetesb_nt, NA, "degrau +0.056 em 100 (ajuste x valor fixo)", "< 0.6% IQA em 98.9-100", "manter; documentado (EQ-05)"),
  list("p_total", "PO4 <= 1", "99 exp(-0.91629 PO4)", "mg/L PO4", SEC, "b_secundaria", pt4, 1, "IBEAS 0.01 P -> 96.30; 0.05 -> 86.03", "-", "manter"),
  list("p_total", "1 < PO4 <= 5", "57.6 - 20.178 PO4 + 2.1326 PO4^2", "mg/L PO4", SEC, "b_secundaria", pt4, 5, "IBEAS 0.44 P -> 34.30", "-", "manter"),
  list("p_total", "5 < PO4 <= 10", "19.8 exp(-0.13544 PO4)", "mg/L PO4", SEC, "b_secundaria", pt4, 10, "-", "-", "manter"),
  list("p_total", "PO4 > 10", "5", "mg/L PO4", CET, "a_oficial_literal", pt4, NA, "-", "-", "manter"),
  list("p_total", "conversao", "PO4 = P x 3.066", "mg/L", "CETESB (2023) Apendice D, p. 5", "a_oficial_literal", NULL, NA, "-", "-", "manter"),
  list("temperatura", "padrao", "qi = 94 constante", "-", "CPRH; IBEAS ('constante de 94 pela CETESB'); CETESB p. 5 (q^w = 1.58)", "b_secundaria", NULL, NA, "94^0.1 = 1.5751 ~ 1.58", "-", "manter"),
  list("temperatura", "dT < -5", "indefinido (NA)", "C", CET, "a_oficial_literal", tmp, NA, "v0.10.0 retornava 93", "so modos experimentais", "corrigido"),
  list("temperatura", "-5 <= dT <= 15 (delta/reference)", "100 - 1.4|dT| - 0.04dT^2; 100 - 3.8dT + 0.1dT^2; 90.5 - 4.5(dT - 5)", "C", "nenhuma", "d_sem_fonte", tmp, 5, "diverge da curva q6 (pico ~93 em 0; ~30 em 10; codigo 100 e 68)", "so modos experimentais", "experimental; aviso; decisao do mantenedor (EQ-07/08)"),
  list("temperatura", "dT > 15", "9", "C", CET, "a_oficial_literal", tmp, NA, "-", "-", "manter"),
  list("turbidez", "0 <= T <= 25", "100.17 - 2.67 T + 0.03775 T^2", "UNT", SEC, "b_secundaria", .qi_cetesb_turbidez, 25, "IBEAS 16.6 -> 66.30; 20 -> 61.87", "-", "manter"),
  list("turbidez", "25 < T <= 100", "84.76 exp(-0.016206 T)", "UNT", SEC, "b_secundaria", .qi_cetesb_turbidez, 100, "-", "-", "manter"),
  list("turbidez", "T > 100", "5", "UNT", CET, "a_oficial_literal", .qi_cetesb_turbidez, NA, "degrau 16.8 -> 5 e do valor fixo oficial", "-", "manter"),
  list("solidos_totais", "0 <= C <= 150", "79.75 + 0.166 C - 0.001088 C^2", "mg/L", SEC, "b_secundaria", .qi_cetesb_solidos_totais, 150, "IBEAS 100 -> 85.47", "-", "manter"),
  list("solidos_totais", "150 < C <= 500", "101.67 - 0.13917 C", "mg/L", SEC, "b_secundaria", .qi_cetesb_solidos_totais, 500, "IBEAS 282 -> 62.40", "-", "manter"),
  list("solidos_totais", "C > 500", "32", "mg/L", CET, "a_oficial_literal", .qi_cetesb_solidos_totais, NA, "-", "-", "manter")
)
res <- do.call(rbind, lapply(m, function(r) {
  f <- r[[7]]; b <- r[[8]]
  le <- if (!is.null(f) && !is.na(b)) f(b - eps) else NA
  ld <- if (!is.null(f) && !is.na(b)) f(b + eps) else NA
  data.frame(parametro = r[[1]], faixa = r[[2]], equacao = r[[3]], unidade = r[[4]],
             referencia = r[[5]], status_evidencia = r[[6]], fronteira_superior = b,
             limite_esquerdo = round(le, 6), limite_direito = round(ld, 6),
             erro_versus_referencia = r[[9]], impacto_IQA = r[[10]], acao = r[[11]])
}))
utils::write.csv(res, "inst/audit/MATRIZ_VALIDACAO_QI.csv", row.names = FALSE)
print(table(res$status_evidencia))

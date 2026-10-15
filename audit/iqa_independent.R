# audit/iqa_independent.R
# Implementacao independente do IQA CETESB, escrita a partir da matriz de
# fontes (inst/audit/MATRIZ_VALIDACAO_QI.csv), sem chamar nenhuma funcao do
# pacote para os qi. Compara com iqa() numa grade densa e deterministica.
# Detecta erros de implementacao (fronteiras, bases, ramos, agregacao); NAO
# valida os coeficientes, que sao os mesmos da matriz.
# Saida: audit/resultados/verificacao_independente.csv
#
#   Rscript audit/iqa_independent.R

pw <- function(x, conds, vals) {           # funcao por partes explicita
  out <- rep(NA_real_, length(x))
  for (k in seq_along(conds)) {
    i <- which(is.na(out) & conds[[k]](x))
    out[i] <- vals[[k]](x[i])
  }
  out
}
lim <- function(q) pmin(100, pmax(0, q))

q_od <- function(od, t, h) {
  cs <- (14.62 - 0.3898 * t + 0.006969 * t^2 - 0.00005896 * t^3) * (1 - 0.0000228675 * h)^5.167
  s <- 100 * od / cs
  lim(pw(s, list(function(s) s <= 50, function(s) s <= 85, function(s) s <= 100,
                 function(s) s <= 140, function(s) s > 140),
         list(function(s) 3 + 0.34 * s + 0.008095 * s^2 + 1.35252e-5 * s^3,
              function(s) 3 - 1.166 * s + 0.058 * s^2 - 3.803435e-4 * s^3,
              function(s) 3 + 3.7745 * s^0.704889,
              function(s) 3 + 2.9 * s - 0.02496 * s^2 + 5.60919e-5 * s^3,
              function(s) rep(47, length(s)))))
}
q_cf <- function(c) {
  l <- log10(pmax(c, 1))
  lim(pw(c, list(function(c) c <= 1, function(c) c <= 10, function(c) c <= 1e5, function(c) c > 1e5),
         list(function(c) rep(100, length(c)), function(c) 100 - 33 * log10(c),
              function(c) 100 - 37.2 * log10(c) + 3.60743 * log10(c)^2, function(c) rep(3, length(c)))))
}
q_ph <- function(p) lim(pw(p,
  list(function(p) p <= 2, function(p) p <= 4, function(p) p <= 6.2, function(p) p <= 7,
       function(p) p <= 8, function(p) p <= 8.5, function(p) p <= 9, function(p) p <= 10,
       function(p) p <= 12, function(p) p > 12),
  list(function(p) rep(2, length(p)), function(p) 13.6 - 10.6 * p + 2.4364 * p^2,
       function(p) 155.5 - 77.36 * p + 10.2481 * p^2, function(p) -657.2 + 197.38 * p - 12.9167 * p^2,
       function(p) -427.8 + 142.05 * p - 9.695 * p^2, function(p) 216 - 16 * p,
       function(p) 1415823 * exp(-1.1507 * p), function(p) 288 - 27 * p,
       function(p) 633 - 106.5 * p + 4.5 * p^2, function(p) rep(3, length(p)))))
q_dbo <- function(c) lim(pw(c, list(function(c) c <= 5, function(c) c <= 15, function(c) c <= 30, function(c) c > 30),
  list(function(c) 99.96 * exp(-0.1232728 * c), function(c) 104.67 - 31.5463 * log(c),
       function(c) 4394.91 * c^-1.99809, function(c) rep(2, length(c)))))
q_nt <- function(c) lim(pw(c, list(function(c) c <= 10, function(c) c <= 60, function(c) c <= 100, function(c) c > 100),
  list(function(c) 100 - 8.169 * c + 0.3059 * c^2, function(c) 101.9 - 23.1023 * log(c),
       function(c) 159.3148 * exp(-0.0512842 * c), function(c) rep(1, length(c)))))
q_pt <- function(p) { c <- 3.066 * p
  lim(pw(c, list(function(c) c <= 1, function(c) c <= 5, function(c) c <= 10, function(c) c > 10),
         list(function(c) 99 * exp(-0.91629 * c), function(c) 57.6 - 20.178 * c + 2.1326 * c^2,
              function(c) 19.8 * exp(-0.13544 * c), function(c) rep(5, length(c))))) }
q_tu <- function(t) lim(pw(t, list(function(t) t <= 25, function(t) t <= 100, function(t) t > 100),
  list(function(t) 100.17 - 2.67 * t + 0.03775 * t^2, function(t) 84.76 * exp(-0.016206 * t),
       function(t) rep(5, length(t)))))
q_st <- function(s) lim(pw(s, list(function(s) s <= 150, function(s) s <= 500, function(s) s > 500),
  list(function(s) 79.75 + 0.166 * s - 0.001088 * s^2, function(s) 101.67 - 0.13917 * s,
       function(s) rep(32, length(s)))))

iqa_ind <- function(d) {
  q <- cbind(q_od(d$od, d$temperatura, d$alt)^0.17, q_cf(d$coliformes)^0.15, q_ph(d$ph)^0.12,
             q_dbo(d$dbo)^0.10, q_nt(d$nt_total)^0.10, q_pt(d$p_total)^0.10, 94^0.10,
             q_tu(d$turbidez)^0.08, q_st(d$solidos_totais)^0.08)
  apply(q, 1, prod)
}

# Grade deterministica: cada parametro percorre seu dominio com passos que
# caem dos dois lados das fronteiras; combinacoes por defasagem de indices.
n <- 3000
k <- seq_len(n)
g <- function(lo, hi, step) lo + ((k * step) %% 1) * (hi - lo)
d <- data.frame(
  od = g(0.2, 16, 0.6180340), temperatura = g(5, 35, 0.4142136), alt = g(0, 2000, 0.7320508),
  coliformes = 10^g(0, 6, 0.2360680), ph = g(1.5, 12.5, 0.3166248), dbo = g(0, 40, 0.1622777),
  nt_total = g(0, 120, 0.5615528), p_total = g(0, 4, 0.8284271), turbidez = g(0, 150, 0.0901699),
  solidos_totais = g(0, 700, 0.6457513)
)
ref <- iqa_ind(d)

suppressMessages(devtools::load_all(".", quiet = TRUE))
pk <- vapply(seq_len(n), function(i) {
  o <- iqa(d[i, setdiff(names(d), "alt")], altitude_m = d$alt[i], details = TRUE)
  attr(o, "components")$IQA_unrounded
}, numeric(1))

res <- data.frame(d, IQA_independente = ref, IQA_pacote = pk, dif = pk - ref)
utils::write.csv(res, "audit/resultados/verificacao_independente.csv", row.names = FALSE)
cat("linhas:", n, " max |dif|:", format(max(abs(res$dif)), digits = 3),
    " linhas com |dif| > 1e-9:", sum(abs(res$dif) > 1e-9), "\n")
cls <- function(x) cut(x, c(-Inf, 19, 36, 51, 79, Inf))
cat("classes diferentes:", sum(cls(ref) != cls(pk)), "\n")

# CETESB/NSF Ã¢â‚¬â€œ qi por parÃƒÂ¢metro (equaÃƒÂ§ÃƒÂµes publicadas)
# ReferÃƒÂªncias: curvas e pesos (CETESB, ApÃƒÂªndice D); equaÃƒÂ§ÃƒÂµes qi (compilaÃƒÂ§ÃƒÂ£o citando CETESB/Von Sperling). 
# OD usa % de saturaÃƒÂ§ÃƒÂ£o: Cs = f(temp, altitude). Ver fontes no README.

.qi_do_sat <- function(od, temp, altitude_m = 0){
  Cs <- (14.62 - 0.3898*temp + 0.006969*temp^2 - 0.00005898*temp^3) * (1 - 0.0000228675*altitude_m)^5.167
  psat <- pmax(0, 100 * (od / Cs))
  out <- numeric(length(psat))
  i <- which(psat <= 50); if(length(i)) out[i] <- 3 + 0.34*psat[i] + 0.008095*(psat[i]^2) + 1.35252e-05*(psat[i]^3)
  i <- which(psat > 50 & psat <= 85); if(length(i)) out[i] <- 3 - 1.166*psat[i] + 0.058*(psat[i]^2) - 3.803435e-04*(psat[i]^3)
  i <- which(psat > 85 & psat <= 100); if(length(i)) out[i] <- 3 + 3.7745*(psat[i]^(0.704889))
  i <- which(psat > 100 & psat <= 140); if(length(i)) out[i] <- 3 + 2.9*psat[i] - 0.02496*(psat[i]^2) + 5.60919e-05*(psat[i]^3)
  i <- which(psat > 140); if(length(i)) out[i] <- 47
  pmin(pmax(out, 0), 100)
}

.qi_fc <- function(coliformes){
  logC <- log10(pmax(coliformes, 1e-6))
  out <- numeric(length(logC))
  i <- which(logC >= 0 & logC <= 1); if(length(i)) out[i] <- 100 - 33*logC[i]
  i <- which(logC > 1 & logC <= 5); if(length(i)) out[i] <- 100 - 37.2*logC[i] + 3.60743*(logC[i]^2)
  i <- which(logC > 5); if(length(i)) out[i] <- 3
  pmin(pmax(out, 0), 100)
}

.qi_ph <- function(pH){
  out <- numeric(length(pH)); out[pH <= 2] <- 2
  i <- which(pH > 2 & pH <= 4);  if(length(i)) out[i] <- 13.6 - 10.6*pH[i] + 2.4364*(pH[i]^2)
  i <- which(pH > 4 & pH <= 6.2);if(length(i)) out[i] <- 155.5 - 76.36*pH[i] + 10.2481*(pH[i]^2)
  i <- which(pH > 6.2 & pH <= 7); if(length(i)) out[i] <- -657.2 + 197.38*pH[i] - 12.9167*(pH[i]^2)
  i <- which(pH > 7 & pH <= 8);  if(length(i)) out[i] <- -427.8 + 142.05*pH[i] - 9.695*(pH[i]^2)
  i <- which(pH > 8 & pH <= 8.5);if(length(i)) out[i] <- 216 - 16*pH[i]
  i <- which(pH > 8.5 & pH <= 9); if(length(i)) out[i] <- 1415823 * (10^(-1.1507*pH[i]))
  i <- which(pH > 9 & pH <= 10); if(length(i)) out[i] <- 50 - 32*(pH[i]-9)
  i <- which(pH > 10 & pH <= 12);if(length(i)) out[i] <- 633 - 106.5*pH[i] + 4.5*(pH[i]^2)
  out[pH > 12] <- 3
  pmin(pmax(out, 0), 100)
}

.qi_dbo <- function(dbo){
  C <- pmax(dbo, 0); out <- numeric(length(C))
  i <- which(C >= 0 & C <= 5);  if(length(i)) out[i] <- 99*(10^(-0.1232728*C[i]))
  i <- which(C > 5 & C <= 15);  if(length(i)) out[i] <- 104.67 - 31.5463*log10(C[i])
  i <- which(C > 15 & C <= 30); if(length(i)) out[i] <- 4394.91*(C[i]**(-1.99809))
  out[C > 30] <- 2
  pmin(pmax(out, 0), 100)
}

.qi_nt <- function(nt){
  C <- pmax(nt, 0); out <- numeric(length(C))
  i <- which(C >= 0 & C <= 10);  if(length(i)) out[i] <- 100 - 8.169*C[i] + 0.3059*(C[i]^2)
  i <- which(C > 10 & C <= 60);  if(length(i)) out[i] <- 101.9 - 23.1023*log10(C[i])
  i <- which(C > 60 & C <= 100); if(length(i)) out[i] <- 159.3148*(10**(-0.0512842*C[i]))
  out[C > 100] <- 1
  pmin(pmax(out, 0), 100)
}

.qi_pt <- function(pt){
  C <- pmax(pt, 0); out <- numeric(length(C))
  i <- which(C >= 0 & C <= 1);  if(length(i)) out[i] <- 99*(10**(-0.91629*C[i]))
  i <- which(C > 1 & C <= 5);  if(length(i)) out[i] <- 57.6 - 20.178*C[i] + 2.1326*(C[i]^2)
  i <- which(C > 5 & C <= 10); if(length(i)) out[i] <- 19.8*(10**(-0.13544*C[i]))
  out[C > 10] <- 5
  pmin(pmax(out, 0), 100)
}

.qi_turb <- function(turb){
  T <- pmax(turb, 0); out <- numeric(length(T))
  i <- which(T >= 0 & T <= 150); if(length(i)) out[i] <- 100.17 - 2.67*T[i] + 0.03775*(T[i]^2)
  i <- which(T > 150 & T <= 500);if(length(i)) out[i] <- 84.76*(10**(-0.016206*T[i]))
  out[T > 500] <- 5
  pmin(pmax(out, 0), 100)
}

.qi_tds <- function(tds){
  C <- pmax(tds, 0); out <- numeric(length(C))
  i <- which(C >= 0 & C <= 25); if(length(i)) out[i] <- 79.75 + 0.166*C[i] - 0.001088*(C[i]^2)
  i <- which(C > 25 & C <= 100);if(length(i)) out[i] <- 101.67 - 0.13917*C[i]
  out[C > 100] <- 32
  pmin(pmax(out, 0), 100)
}

#' IQA oficial (produto geomÃƒÂ©trico) com qi CETESB/NSF
#' @param df cols: pH, turbidez, od, dbo, nt_total, p_total, tds, temperatura, coliformes
#' @param pesos pesos oficiais (ANA/CETESB)
#' @param altitude_m altitude (m) p/ %sat de OD
#' @param na_rm reescala pesos nos parÃƒÂ¢metros presentes
#' @export
iqa <- function(df,
  pesos = c(od=.17, coliformes=.15, pH=.12, dbo=.10, nt_total=.10, p_total=.10, turbidez=.08, tds=.08, temperatura=.10),
  altitude_m = 0,
  na_rm = FALSE){

  req <- names(pesos)
  missing <- setdiff(req, names(df))
  if(length(missing) && !na_rm)
    stop("Faltam colunas: ", paste(missing, collapse=", "), ". Use na_rm=TRUE para reescalar.")

  Q <- list()
  if("coliformes" %in% names(df)) Q$coliformes <- .qi_fc(df$coliformes)
  if("pH" %in% names(df))        Q$pH         <- .qi_ph(df$pH)
  if("dbo" %in% names(df))       Q$dbo        <- .qi_dbo(df$dbo)
  if("nt_total" %in% names(df))  Q$nt_total   <- .qi_nt(df$nt_total)
  if("p_total" %in% names(df))   Q$p_total    <- .qi_pt(df$p_total)
  if("turbidez" %in% names(df))  Q$turbidez   <- .qi_turb(df$turbidez)
  if("tds" %in% names(df))       Q$tds        <- .qi_tds(df$tds)

  if(all(c("od","temperatura") %in% names(df))) {
    Q$od <- .qi_do_sat(df$od, df$temperatura, altitude_m)
  } else if ("temperatura" %in% names(df)) {
    Q$temperatura <- rep(94, nrow(df))  # suposiÃƒÂ§ÃƒÂ£o CETESB para ÃŽâ€T
  }

  qi_df <- as.data.frame(Q)
  use <- intersect(names(pesos), names(qi_df))
  qi_df <- qi_df[use]; w <- pesos[use]

  IQA <- rep(NA_real_, nrow(df))
  for(i in seq_len(nrow(df))){
    qi_row <- unlist(qi_df[i, , drop=TRUE]); ok <- !is.na(qi_row)
    if(!any(ok)){ next }
    ww <- w[ok]; ww <- ww / sum(ww)
    IQA[i] <- prod( (qi_row[ok]) ^ ww )
  }
  out <- df; out$IQA <- IQA; out
}

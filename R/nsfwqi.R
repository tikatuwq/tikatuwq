#' NSF Water Quality Index (prototype v0.2.1)
#'
#' @param df Data frame containing columns compatible with NSFWQI mapping
#' @param pesos Named numeric vector with parameter weights
#' @param na_rm Logical; allow NA and rescale weights
#' @return Data frame with an added 'NSFWQI' column
#' @export
nsfwqi <- function(df,
  pesos = c(do=0.17, fc=0.16, ph=0.11, bod=0.11, temp_change=0.10,
            po4=0.10, no3=0.10, turbidez=0.08, sst=0.07),
  na_rm = FALSE){

  qi_fun <- function(param, v){
    if (is.na(v)) return(NA_real_)
    switch(param,
      "do"          = ifelse(v >= 7.5, 90, ifelse(v >= 6, 80, ifelse(v >= 5, 70, 50))),
      "fc"          = ifelse(v <= 200, 90, ifelse(v <= 1000, 70, 40)),
      "ph"          = ifelse(v >= 6.5 & v <= 8.5, 90, 60),
      "bod"         = ifelse(v <= 3, 90, ifelse(v <= 5, 75, 55)),
      "temp_change" = ifelse(v <= 2, 90, ifelse(v <= 5, 75, 55)),
      "po4"         = ifelse(v <= 0.05, 90, ifelse(v <= 0.1, 75, 55)),
      "no3"         = ifelse(v <= 1, 90, ifelse(v <= 10, 70, 50)),
      "turbidez"    = ifelse(v <= 5, 90, ifelse(v <= 50, 70, 50)),
      "sst"         = ifelse(v <= 500, 85, 60),
      50
    )
  }

  req <- intersect(names(pesos), names(df))
  if(!length(req)) stop("No available columns for NSFWQI among: ", paste(names(pesos), collapse=", "))

  qi <- lapply(req, function(p) vapply(df[[p]], function(x) qi_fun(p, x), numeric(1)))
  qi <- as.data.frame(qi); names(qi) <- req

  if (!na_rm && any(is.na(qi))) stop("There are NA values in parameters. Use na_rm=TRUE to ignore incomplete rows.")

  w <- pesos[req]; w <- w / sum(w)
  nsf_val <- as.numeric(qi %*% w)

  dplyr::mutate(df, NSFWQI = nsf_val)
}

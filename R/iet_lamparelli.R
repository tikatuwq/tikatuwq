#' IET - Lamparelli (estrutura v0.2.1)
#' @export
iet_lamparelli <- function(tp = NULL, chla = NULL, sd = NULL, ambiente = c('rio','reservatorio')){
  ambiente <- match.arg(ambiente)
  res <- list()
  if(!is.null(tp))   res$IET_TP   <- 10 + 10*log10(pmax(tp, 0.001))
  if(!is.null(chla)) res$IET_Chla <- 10 + 10*log10(pmax(chla, 0.001))
  if(!is.null(sd))   res$IET_Secchi <- 60 - 14.41 * log10(pmax(sd, 0.001))
  df <- as.data.frame(res)
  if(ncol(df) > 1){
    df$IET_Lamp <- rowMeans(df, na.rm = TRUE)
  } else if(ncol(df) == 1){
    df$IET_Lamp <- df[[1]]
  } else {
    df <- data.frame(IET_Lamp = numeric(0))
  }
  df$ambiente <- ambiente
  df
}

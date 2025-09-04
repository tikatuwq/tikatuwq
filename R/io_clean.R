#' Leitura padrÃƒÂ£o de dados de qualidade da ÃƒÂ¡gua
#' @export
read_wq <- function(path, locale = readr::locale(decimal_mark = ","), tz = "America/Bahia"){
  df <- readr::read_csv(path, locale = locale, show_col_types = FALSE)
  names(df) <- stringr::str_to_lower(names(df))
  if("data" %in% names(df)){
    suppressWarnings({ df$data <- as.Date(df$data) })
  }
  df
}

#' Padroniza/unifica unidades (esqueleto)
#' @export
clean_units <- function(df, units_map = NULL){ df }

#' Valida presenÃƒÂ§a de colunas mÃƒÂ­nimas
#' @export
validate_wq <- function(df, required = c("ph","turbidez","od","dbo","nt_total","p_total","tds","temperatura","coliformes")){
  miss <- setdiff(required, names(df))
  if(length(miss)) stop("Colunas ausentes: ", paste(miss, collapse=", "))
  df
}

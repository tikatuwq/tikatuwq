# R/utils_sanitize.R
# Helpers internos de saneamento de entrada
# (ASCII-only; nao exportar)

# ------------------------------------------------------------------------------
# Dicionario central de aliases de parametros
# ------------------------------------------------------------------------------
# Estrutura: nome_canonico -> vetor de aliases aceitos (ordem = prioridade)
# Todas as funcoes do pacote devem usar .resolve_col() em vez de if/else manual.
# Para adicionar novo alias: basta editar esta lista.
#' @keywords internal
#' @noRd
.param_aliases <- list(
  # Oxigenio dissolvido
  od              = c("od", "do", "dissolved_oxygen", "oxigenio_dissolvido",
                      "o2_dissolvido", "o_d", "od_mgl"),
  # pH
  ph              = c("ph", "pH", "PH"),
  # Demanda bioquimica de oxigenio
  dbo             = c("dbo", "bod", "dbo5", "bod5", "dbo_5", "bod_5",
                      "demanda_bioquimica"),
  # Turbidez
  turbidez        = c("turbidez", "turbidity", "turb", "ntu"),
  # Coliformes termotolerantes / E. coli
  coliformes      = c("coliformes", "col_termotolerantes", "e_coli", "ecoli",
                      "fc", "fecal_coliform", "coliforms", "ct"),
  # Solidos totais dissolvidos
  tds             = c("tds", "sd", "solidos_dissolvidos", "total_dissolved_solids",
                      "dissolved_solids"),
  # Temperatura
  temperatura     = c("temperatura", "temperature", "temp", "temp_c"),
  # Fosforo total (mg/L)
  p_total         = c("p_total", "ptotal", "fosforo_total", "phosphorus",
                      "tp_mgl", "total_phosphorus", "p_tot"),
  # Fosforo total em ug/L (IET usa esta escala)
  tp_ugl          = c("tp", "tp_ugl"),
  # Nitrogenio total
  nt_total        = c("nt_total", "n_total", "ntk", "nkjeldahl",
                      "nitrogenio_total", "total_nitrogen", "tn"),
  # Nitrato
  n_nitrato       = c("n_nitrato", "no3", "nitrato", "nitrate", "n_no3"),
  # Nitrito
  n_nitrito       = c("n_nitrito", "no2", "nitrito", "nitrite", "n_no2"),
  # Amonia / nitrogenio amoniacal
  amonia          = c("amonia", "n_amoniacal", "nh3", "nh4", "ammonia",
                      "ammoniacal_nitrogen"),
  # Solidos suspensos totais
  solidos_suspensos = c("solidos_suspensos", "sst", "ss", "tss",
                        "total_suspended_solids", "suspended_solids"),
  # Solidos totais
  solidos_totais  = c("solidos_totais", "st", "ts", "total_solids"),
  # Condutividade eletrica
  condutividade   = c("condutividade", "ec", "conducao", "conductivity",
                      "cond", "ec_uscm"),
  # Clorofila-a
  clorofila       = c("clorofila", "chla", "chl_a", "chlorophyll",
                      "chlorophyll_a", "clorofila_a"),
  # Disco de Secchi / profundidade de visibilidade
  secchi          = c("secchi", "sd_secchi", "disco_secchi", "secchi_depth",
                      "visibilidade"),
  # Ortofosfato
  p_ortofosfato   = c("p_ortofosfato", "po4", "orthophosphate",
                      "phosphate", "ortofosfato"),
  # Vazao
  vazao           = c("vazao", "flow", "discharge", "q_m3s", "q"),
  # Coordenadas
  lat             = c("lat", "latitude", "y"),
  lon             = c("lon", "longitude", "long", "x")
)

# Vetor inverso: alias -> canonico (pre-computado para lookup O(1))
#' @keywords internal
#' @noRd
.alias_to_canonical <- local({
  result <- character(0)
  for (canonical in names(.param_aliases)) {
    aliases <- .param_aliases[[canonical]]
    names(aliases) <- aliases
    result[aliases] <- canonical
  }
  result
})

#' Resolve nome canonico de coluna em um data frame
#'
#' Dado o nome canonico de um parametro (ex.: "od"), retorna o nome real da
#' coluna encontrada no data frame, testando todos os aliases conhecidos.
#' Retorna NULL se nenhum alias for encontrado.
#'
#' @param df   Data frame de entrada.
#' @param canonical Nome canonico do parametro (chave em .param_aliases).
#' @param required Logico; se TRUE lanca erro quando nao encontrado.
#' @return Nome da coluna no df, ou NULL.
#' @keywords internal
#' @noRd
.resolve_col <- function(df, canonical, required = FALSE) {
  aliases <- .param_aliases[[canonical]]
  if (is.null(aliases)) aliases <- canonical  # parametro desconhecido: tenta direto
  found <- aliases[aliases %in% names(df)]
  if (length(found) == 0L) {
    if (required) stop("Column not found for parameter '", canonical,
                       "'. Expected one of: ", paste(aliases, collapse = ", "))
    return(NULL)
  }
  found[[1L]]  # retorna o primeiro alias encontrado (ordem = prioridade)
}

#' Normaliza nomes de colunas de um df para os nomes canonicos do pacote
#'
#' Renomeia colunas cujos nomes sejam aliases conhecidos para o nome canonico.
#' Util como primeiro passo em leitura de dados externos.
#'
#' @param df Data frame de entrada.
#' @return O df com colunas renomeadas para nomes canonicos quando reconhecidas.
#' @export
#' @examples
#' d <- data.frame(pH = 7, BOD = 3, DO = 6)
#' normalize_param_names(d)
normalize_param_names <- function(df) {
  nms <- names(df)
  for (i in seq_along(nms)) {
    canonical <- .alias_to_canonical[nms[i]]
    if (!is.na(canonical) && canonical != nms[i]) {
      nms[i] <- canonical
    }
  }
  names(df) <- nms
  df
}

#' @keywords internal
#' @noRd
.numify <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.numeric(x)) return(x)
  if (is.factor(x)) x <- as.character(x)
  x <- gsub("\\s", "", x)
  x <- gsub(",", ".", x, fixed = TRUE)  # virgula -> ponto
  x <- gsub("[<>]", "", x)              # remove sinais de desigualdade
  suppressWarnings(as.numeric(x))
}

#' Parse valores censurados (ND/<LD/<LOQ)
#'
#' @description
#' Helper interno que detecta e trata valores censurados (ex.: "<0.01", "<LD", "<LOQ").
#'
#' @param x Vetor de entrada (character ou numeric).
#' @param ld_policy Politica a aplicar: "ld2" (metade do limite), "ld" (limite),
#'   "zero" (0), "na" (NA_real_).
#' @return Vetor numerico com valores tratados conforme politica.
#' @keywords internal
#' @noRd
.parse_nd_ld <- function(x, ld_policy = c("ld2", "ld", "zero", "na")) {
  ld_policy <- match.arg(ld_policy)
  
  if (is.null(x) || length(x) == 0) return(x)
  if (is.numeric(x)) return(x)
  
  x_char <- as.character(x)
  x_out <- rep(NA_real_, length(x_char))
  censored_flags <- logical(length(x_char))
  
  for (i in seq_along(x_char)) {
    if (is.na(x_char[i])) {
      x_out[i] <- NA_real_
      next
    }
    
    trimmed <- trimws(x_char[i])
    
    # Tenta extrair valor numerico de padrao <valor>
    match_val <- grepl("^\\s*[<>]\\s*([0-9]+[.,]?[0-9]*)\\s*$", trimmed, perl = TRUE)
    if (match_val) {
      # Extrai o numero (substitui virgula por ponto)
      num_str <- gsub("^\\s*[<>]\\s*([0-9]+)[.,]([0-9]*)\\s*$", "\\1.\\2", trimmed, perl = TRUE)
      num_str <- gsub("^\\s*[<>]\\s*([0-9]+)\\s*$", "\\1", num_str, perl = TRUE)
      num_str <- gsub(",", ".", num_str, fixed = TRUE)
      ld_value <- suppressWarnings(as.numeric(num_str))
      
      if (is.finite(ld_value)) {
        censored_flags[i] <- TRUE
        if (ld_policy == "ld2") {
          x_out[i] <- ld_value / 2
        } else if (ld_policy == "ld") {
          x_out[i] <- ld_value
        } else if (ld_policy == "zero") {
          x_out[i] <- 0
        } else {  # "na"
          x_out[i] <- NA_real_
        }
        next
      }
    }
    
    # Tenta detectar <LD, <LOD, <LOQ, ND sem valor numerico
    is_censored <- grepl("^\\s*[<>]\\s*(LD|LOD|LOQ)\\s*$", trimmed, ignore.case = TRUE, perl = TRUE) ||
                   grepl("^\\s*ND\\s*$", trimmed, ignore.case = TRUE, perl = TRUE) ||
                   grepl("^\\s*[<>]\\s*(LD|LOD|LOQ)\\s+([0-9]+[.,]?[0-9]*)\\s*$", trimmed, ignore.case = TRUE, perl = TRUE)
    
    if (is_censored) {
      # Tenta extrair valor apos LD/LOD/LOQ
      ld_match <- regmatches(trimmed, regexec("^\\s*[<>]\\s*(?:LD|LOD|LOQ)\\s+([0-9]+[.,]?[0-9]*)\\s*$", trimmed, ignore.case = TRUE, perl = TRUE))
      if (length(ld_match[[1]]) > 1) {
        num_str <- gsub(",", ".", ld_match[[1]][2], fixed = TRUE)
        ld_value <- suppressWarnings(as.numeric(num_str))
        if (is.finite(ld_value)) {
          censored_flags[i] <- TRUE
          if (ld_policy == "ld2") {
            x_out[i] <- ld_value / 2
          } else if (ld_policy == "ld") {
            x_out[i] <- ld_value
          } else if (ld_policy == "zero") {
            x_out[i] <- 0
          } else {
            x_out[i] <- NA_real_
          }
          next
        }
      }
      
      # Se nao tem valor numerico, usa 0 ou NA conforme politica
      censored_flags[i] <- TRUE
      if (ld_policy == "zero") {
        x_out[i] <- 0
      } else {
        x_out[i] <- NA_real_
      }
      next
    }
    
    # Se nao e censurado, tenta converter para numero normalmente
    num_str <- gsub(",", ".", trimmed, fixed = TRUE)
    x_out[i] <- suppressWarnings(as.numeric(num_str))
  }
  
  # Se havia censura e politica e "ld2", pode avisar (opcional, silencioso por agora)
  # attr(x_out, "censored") <- censored_flags
  
  x_out
}

# extrai vetores numericos p/ IET (usa dicionario central de aliases)
#' @keywords internal
#' @noRd
.df_extract_iet <- function(df) {
  col_sec  <- .resolve_col(df, "secchi")
  col_chla <- .resolve_col(df, "clorofila")
  col_tp   <- .resolve_col(df, "tp_ugl")
  col_pt   <- .resolve_col(df, "p_total")

  secchi    <- if (!is.null(col_sec))  .numify(df[[col_sec]])  else NULL
  clorofila <- if (!is.null(col_chla)) .numify(df[[col_chla]]) else NULL

  # tp em ug/L: prefere coluna tp direto; converte p_total (mg/L) se necessario
  tp <- if (!is.null(col_tp)) {
    .numify(df[[col_tp]])
  } else if (!is.null(col_pt)) {
    1000 * .numify(df[[col_pt]])   # mg/L -> ug/L
  } else NULL

  ids <- df[, intersect(c("rio","ponto","data","lat","lon"), names(df)), drop = FALSE]
  list(secchi = secchi, clorofila = clorofila, tp = tp, ids = ids)
}

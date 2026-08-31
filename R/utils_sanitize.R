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
  od                      = c("od", "do", "dissolved_oxygen", "oxigenio_dissolvido",
                              "o2_dissolvido", "o_d", "od_mgl"),
  # pH
  ph                      = c("ph", "pH", "PH"),
  # Demanda bioquimica de oxigenio
  dbo                     = c("dbo", "bod", "dbo5", "bod5", "dbo_5", "bod_5",
                              "demanda_bioquimica"),
  # Turbidez
  turbidez                = c("turbidez", "turbidity", "turb", "ntu"),
  # Coliformes termotolerantes
  coliformes_termotolerantes = c("coliformes_termotolerantes", "col_termotolerantes",
                                 "coliformes", "fc", "fecal_coliform", "coliforms", "ct"),
  # E. coli (especifico)
  e_coli                  = c("e_coli", "ecoli", "escherichia_coli"),
  # Solidos totais (IQA CETESB oficial)
  solidos_totais          = c("solidos_totais", "st", "ts", "total_solids",
                              "residuo_total", "solidos_total"),
  # Solidos totais dissolvidos (TDS - NAO e Solidos Totais)
  tds                     = c("tds", "sd", "solidos_dissolvidos", "total_dissolved_solids",
                              "dissolved_solids"),
  # Temperatura absoluta da agua (usada na saturacao de OD)
  temperatura             = c("temperatura", "temp", "temp_c", "water_temperature",
                              "temperatura_agua", "temperature"),
  # Variacao de temperatura / Afastamento termico (Delta T)
  delta_temperatura       = c("delta_temperatura", "delta_t", "temp_change",
                              "temperature_change", "variacao_temperatura", "delta_temp"),
  # Altitude
  altitude                = c("altitude", "altitude_m", "alt", "elevacao"),
  # Fosforo total (mg/L)
  p_total                 = c("p_total", "ptotal", "fosforo_total", "phosphorus",
                              "tp_mgl", "total_phosphorus", "p_tot"),
  # Fosforo total em ug/L (IET usa esta escala)
  tp_ugl                  = c("tp", "tp_ugl"),
  # Nitrogenio total
  nt_total                = c("nt_total", "n_total", "ntk", "nkjeldahl",
                              "nitrogenio_total", "total_nitrogen", "tn"),
  # Nitrato
  n_nitrato               = c("n_nitrato", "no3", "nitrato", "nitrate", "n_no3"),
  # Nitrito
  n_nitrito               = c("n_nitrito", "no2", "nitrito", "nitrite", "n_no2"),
  # Amonia / nitrogenio amoniacal
  amonia                  = c("amonia", "n_amoniacal", "nh3", "nh4", "ammonia",
                              "ammoniacal_nitrogen"),
  # Solidos suspensos totais
  solidos_suspensos       = c("solidos_suspensos", "sst", "ss", "tss",
                              "total_suspended_solids", "suspended_solids"),
  # Condutividade eletrica
  condutividade           = c("condutividade", "ec", "conducao", "conductivity",
                              "cond", "ec_uscm"),
  # Clorofila-a
  clorofila               = c("clorofila", "chla", "chl_a", "chlorophyll",
                              "chlorophyll_a", "clorofila_a"),
  # Disco de Secchi / profundidade de visibilidade
  secchi                  = c("secchi", "sd_secchi", "disco_secchi", "secchi_depth",
                              "visibilidade"),
  # Ortofosfato
  p_ortofosfato           = c("p_ortofosfato", "po4", "orthophosphate",
                              "phosphate", "ortofosfato"),
  # Vazao
  vazao                   = c("vazao", "flow", "discharge", "q_m3s", "q"),
  # Coordenadas
  lat                     = c("lat", "latitude", "y"),
  lon                     = c("lon", "longitude", "long", "x")
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
#' @param censor_policy Politica a aplicar: "limit" (valor do limite), "half_limit" (metade do limite),
#'   "zero" (0), "na" (NA_real_), "preserve" (preserva original), "ld2" (alias de half_limit), "ld" (alias de limit).
#' @return Vetor numerico (ou character quando preserve) com valores tratados conforme politica.
#' @keywords internal
#' @noRd
.parse_nd_ld <- function(x, censor_policy = c("limit", "half_limit", "zero", "na", "preserve", "ld2", "ld"), ld_policy = NULL) {
  if (!is.null(ld_policy)) censor_policy <- ld_policy
  censor_policy <- match.arg(censor_policy, c("limit", "half_limit", "zero", "na", "preserve", "ld2", "ld"))
  if (censor_policy == "ld2") censor_policy <- "half_limit"
  if (censor_policy == "ld")  censor_policy <- "limit"

  if (is.null(x) || length(x) == 0) return(x)
  if (censor_policy == "preserve") return(x)
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

    # Tenta extrair valor numerico de padrao <valor> ou >valor
    match_val <- grepl("^\\s*[<>]\\s*([0-9]+[.,]?[0-9]*)\\s*$", trimmed, perl = TRUE)
    if (match_val) {
      num_str <- gsub("^\\s*[<>]\\s*([0-9]+)[.,]([0-9]*)\\s*$", "\\1.\\2", trimmed, perl = TRUE)
      num_str <- gsub("^\\s*[<>]\\s*([0-9]+)\\s*$", "\\1", num_str, perl = TRUE)
      num_str <- gsub(",", ".", num_str, fixed = TRUE)
      ld_value <- suppressWarnings(as.numeric(num_str))

      if (is.finite(ld_value)) {
        censored_flags[i] <- TRUE
        if (censor_policy == "half_limit") {
          x_out[i] <- ld_value / 2
        } else if (censor_policy == "limit") {
          x_out[i] <- ld_value
        } else if (censor_policy == "zero") {
          x_out[i] <- 0
        } else {  # "na"
          x_out[i] <- NA_real_
        }
        next
      }
    }

    # Tenta detectar <LD, <LOD, <LOQ, ND
    is_censored <- grepl("^\\s*[<>]\\s*(LD|LOD|LOQ)\\s*$", trimmed, ignore.case = TRUE, perl = TRUE) ||
                   grepl("^\\s*ND\\s*$", trimmed, ignore.case = TRUE, perl = TRUE) ||
                   grepl("^\\s*[<>]\\s*(LD|LOD|LOQ)\\s+([0-9]+[.,]?[0-9]*)\\s*$", trimmed, ignore.case = TRUE, perl = TRUE)

    if (is_censored) {
      ld_match <- regmatches(trimmed, regexec("^\\s*[<>]\\s*(?:LD|LOD|LOQ)\\s+([0-9]+[.,]?[0-9]*)\\s*$", trimmed, ignore.case = TRUE, perl = TRUE))
      if (length(ld_match[[1]]) > 1) {
        num_str <- gsub(",", ".", ld_match[[1]][2], fixed = TRUE)
        ld_value <- suppressWarnings(as.numeric(num_str))
        if (is.finite(ld_value)) {
          censored_flags[i] <- TRUE
          if (censor_policy == "half_limit") {
            x_out[i] <- ld_value / 2
          } else if (censor_policy == "limit") {
            x_out[i] <- ld_value
          } else if (censor_policy == "zero") {
            x_out[i] <- 0
          } else {
            x_out[i] <- NA_real_
          }
          next
        }
      }

      censored_flags[i] <- TRUE
      if (censor_policy == "zero") {
        x_out[i] <- 0
      } else {
        x_out[i] <- NA_real_
      }
      next
    }

    # Se nao e censurado, tenta converter para numero
    num_str <- gsub(",", ".", trimmed, fixed = TRUE)
    x_out[i] <- suppressWarnings(as.numeric(num_str))
  }

  attr(x_out, "censored") <- censored_flags
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

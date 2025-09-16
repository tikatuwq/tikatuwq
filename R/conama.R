# R/conama.R
# ASCII-only in code; keep messages/data UTF-8 but avoid non-ASCII in errors if needed.

#' Limits for Brazilian CONAMA 357/2005
#'
#' @description Returns the parameter limits defined by CONAMA Resolution 357/2005
#'   for a given water-use class.
#'
#' @param class Integer or character. Target class (e.g., 1, 2, 3, 4 or "special"),
#'   according to CONAMA 357/2005.
#'
#' @return A tibble/data frame with one row per parameter and the regulatory thresholds.
#'   Typical columns:
#'   \itemize{
#'     \item \code{parameter}: parameter name (character)
#'     \item \code{class}: CONAMA class label (character/integer)
#'     \item \code{limit_min}: minimum allowed value (numeric, may be \code{NA})
#'     \item \code{limit_max}: maximum allowed value (numeric, may be \code{NA})
#'     \item \code{unit}: measurement unit (character)
#'     \item \code{criterion}: textual rule if applicable (character)
#'   }
#'
#' @examples
#' # class 2 thresholds
#' head(conama_limits(2))
#'
#' @export
conama_limits <- function(class){
  path <- system.file("extdata","conama_limits.csv", package = "tikatuwq")
  df <- readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
  df$classe <- as.character(df$classe)
  # normaliza nomes de parametros para casar com colunas do df
  df$parametro <- df$parametro |>
    tolower() |>
    stringr::str_replace_all("\\s+", "_") |>
    stringr::str_replace_all("[^a-z0-9_]", "")
  if (missing(class)) return(df)
  # filtra pela classe informada (aceita numero/char)
  df[as.character(df$classe) == as.character(class), , drop = FALSE]
}


# Helpers internos -------------------------------------------------------------

# pega, de um data.frame de limites (linhas do parametro), os valores min/max
.get_minmax <- function(row) {
  nms <- names(row)
  # tenta diferentes nomes possiveis
  minv <- NA_real_
  maxv <- NA_real_
  mmn <- intersect(c("min", "minimo", "lim_min"), nms)
  mxn <- intersect(c("max", "maximo", "lim_max"), nms)
  if (length(mmn)) minv <- suppressWarnings(as.numeric(row[[mmn[1]]]))
  if (length(mxn)) maxv <- suppressWarnings(as.numeric(row[[mxn[1]]]))
  list(min = minv, max = maxv)
}

# calcula status/delta dado valor e min/max
.status_delta_one <- function(val, minv, maxv) {
  if (is.na(val))          return(list(status = NA_character_, delta = NA_real_))
  if (!is.na(minv) && !is.na(maxv)) {
    if (val < minv) return(list(status = "abaixo_do_minimo", delta = val - minv))
    if (val > maxv) return(list(status = "acima_do_maximo",  delta = val - maxv))
    return(list(status = "ok", delta = 0))
  } else if (!is.na(minv)) {
    if (val < minv) return(list(status = "abaixo_do_minimo", delta = val - minv))
    return(list(status = "ok", delta = 0))
  } else if (!is.na(maxv)) {
    if (val > maxv) return(list(status = "acima_do_maximo",  delta = val - maxv))
    return(list(status = "ok", delta = 0))
  } else {
    return(list(status = NA_character_, delta = NA_real_))
  }
}

#' Conformity check (default Class = "2") - detailed
#'
#' For each parameter present in `df`, adds columns:
#' - `*_ok` (TRUE/FALSE),
#' - `*_status` ("ok", "acima_do_maximo", "abaixo_do_minimo"),
#' - `*__lim_min` and `*__lim_max` (limits used),
#' - `*__delta` (difference to the relevant limit; >0 if above max,
#'   <0 if below min, 0 if ok).
#'
#' Se existirem multiplas linhas de limite para o mesmo parametro, a funcao:
#' - considera `*_ok` como TRUE se QUALQUER linha for atendida; e
#' - escolhe, para `status/lim_min/lim_max/delta`, a primeira linha que atende;
#'   se nenhuma atende, escolhe a que produz a menor violacao (menor |delta|).
#'
#' @param df tibble/data.frame com colunas de parametros (ph, turbidez, od, dbo, etc.).
#' @param classe string da classe ("especial", "1", "2", "3", "4" ou conforme tabela).
#' @return `df` com colunas adicionais por parametro.
#' @export
conama_check <- function(df, classe = "2") {
  lim_all <- conama_limits()
  lim <- lim_all[as.character(lim_all$classe) == as.character(classe), , drop = FALSE]
  if (!nrow(lim)) return(df)

  out <- df
  params_present <- intersect(unique(lim$parametro), names(df))
  if (!length(params_present)) return(out)

  for (p in params_present) {
    v <- out[[p]]

    # garantir numerico (usa helper do leitor, se existir)
    if (!is.numeric(v)) {
      v_num <- suppressWarnings(as.numeric(v))
      if (all(is.na(v_num))) {
        if (exists(".to_number_auto", mode = "function")) {
          v_num <- .to_number_auto(v)
        } else {
          v_num <- suppressWarnings(as.numeric(v))
        }
      }
      v <- v_num
    }

    rows <- lim[lim$parametro == p, , drop = FALSE]
    if (!nrow(rows)) next

    n <- length(v)
    k <- nrow(rows)

    ok_mat     <- matrix(FALSE, nrow = n, ncol = k)
    lim_min_m  <- matrix(NA_real_, nrow = n, ncol = k)
    lim_max_m  <- matrix(NA_real_, nrow = n, ncol = k)
    delta_m    <- matrix(NA_real_, nrow = n, ncol = k)
    status_m   <- matrix(NA_character_, nrow = n, ncol = k)

    for (j in seq_len(k)) {
      mm <- .get_minmax(rows[j, , drop = FALSE])
      minv <- mm$min
      maxv <- mm$max
      lim_min_m[, j] <- rep(if (!is.na(minv)) minv else NA_real_, n)
      lim_max_m[, j] <- rep(if (!is.na(maxv)) maxv else NA_real_, n)

      # ok por linha de limite
      ok_j <- (is.na(minv) | (!is.na(v) & v >= minv)) &
              (is.na(maxv) | (!is.na(v) & v <= maxv))
      ok_mat[, j] <- ok_j

      # status/delta por linha
      for (i in seq_len(n)) {
        sd <- .status_delta_one(v[i], minv, maxv)
        status_m[i, j] <- sd$status
        delta_m[i, j]  <- sd$delta
        if (ok_j[i]) {
          # normaliza delta ok para 0
          delta_m[i, j] <- 0
        }
      }
    }

    # combinacao por observacao: escolhe a linha aplicavel
    ok_any <- apply(ok_mat, 1, any)
    # indice da primeira linha ok (ou NA se nenhuma)
    first_ok_idx <- apply(ok_mat, 1, function(x) { w <- which(x); if (length(w)) w[1] else NA_integer_ })

    # vetores finais
    lim_min_vec <- rep(NA_real_, n)
    lim_max_vec <- rep(NA_real_, n)
    status_vec  <- rep(NA_character_, n)
    delta_vec   <- rep(NA_real_, n)

    for (i in seq_len(n)) {
      if (isTRUE(ok_any[i])) {
        j <- first_ok_idx[i]
        lim_min_vec[i] <- lim_min_m[i, j]
        lim_max_vec[i] <- lim_max_m[i, j]
        status_vec[i]  <- "ok"
        delta_vec[i]   <- 0
      } else {
        # nenhuma linha atende: escolha pela menor |delta|
        deltas <- delta_m[i, ]
        if (all(is.na(deltas))) {
          # nao ha como decidir
          lim_min_vec[i] <- NA_real_
          lim_max_vec[i] <- NA_real_
          status_vec[i]  <- NA_character_
          delta_vec[i]   <- NA_real_
        } else {
          j <- which.min(abs(deltas))
          lim_min_vec[i] <- lim_min_m[i, j]
          lim_max_vec[i] <- lim_max_m[i, j]
          status_vec[i]  <- status_m[i, j]
          delta_vec[i]   <- deltas[j]
        }
      }
    }

    out[[paste0(p, "_ok")]]       <- as.logical(ok_any)
    out[[paste0(p, "_status")]]   <- status_vec
    out[[paste0(p, "__lim_min")]] <- lim_min_vec
    out[[paste0(p, "__lim_max")]] <- lim_max_vec
    out[[paste0(p, "__delta")]]   <- delta_vec
  }

  out
}

# (Opcional) resumo em formato "longo" -----------------------------------------

#' Conformity summary (long format)
#' @param df dados de entrada
#' @param classe classe CONAMA
#' @return tibble com: parametro, valor, lim_min, lim_max, status, ok, delta
#' @export
conama_summary <- function(df, classe = "2") {
  chk <- conama_check(df, classe = classe)
  lim <- conama_limits()
  lim <- lim[as.character(lim$classe) == as.character(classe), , drop = FALSE]

  params_present <- intersect(unique(lim$parametro), names(df))
  if (!length(params_present)) {
    return(tibble::tibble(
      parametro = character(), valor = numeric(),
      lim_min = numeric(), lim_max = numeric(),
      status = character(), ok = logical(), delta = numeric()
    ))
  }

  purrr::map_dfr(params_present, function(p) {
    tibble::tibble(
      parametro = p,
      valor     = suppressWarnings(as.numeric(chk[[p]])),
      lim_min   = suppressWarnings(as.numeric(chk[[paste0(p, "__lim_min")]])),
      lim_max   = suppressWarnings(as.numeric(chk[[paste0(p, "__lim_max")]])),
      status    = as.character(chk[[paste0(p, "_status")]]),
      ok        = as.logical(chk[[paste0(p, "_ok")]]),
      delta     = suppressWarnings(as.numeric(chk[[paste0(p, "__delta")]]))
    )
  })
}

#' Conformity report (table)
#' @param df dados de entrada
#' @param classe classe CONAMA (ex.: "2")
#' @param only_violations se TRUE, retorna apenas linhas com status != "ok"
#' @return tibble com: parametro, valor, lim_min, lim_max, status, delta
#' @export
conama_report <- function(df, classe = "2", only_violations = TRUE) {
  tb <- conama_summary(df, classe = classe)
  if (only_violations) {
    tb <- tb[!is.na(tb$status) & tb$status != "ok", , drop = FALSE]
  }
  tb[order(tb$parametro), ]
}

#' Text summary of conformity (bulleted, formatted)
#' @param df dados de entrada
#' @param classe classe CONAMA
#' @param only_violations se TRUE, lista apenas parametros com violacao
#' @param decimal_mark separador decimal (default ",")
#' @param big_mark separador de milhar (default ".")
#' @return character vector (linhas)
#' @export
conama_text <- function(df, classe = "2",
                        only_violations = FALSE,
                        decimal_mark = ",", big_mark = ".") {
  tb <- conama_summary(df, classe = classe)
  if (!nrow(tb)) return("Sem parametros aplicaveis para a classe informada.")
  tb <- dplyr::arrange(tb, parametro)

  # casas decimais "inteligentes": inteiros sem ,0; 0.1 -> 0,1; 0.05 -> 0,05; 0.123 -> 0,123
  decimals_for_one <- function(x) {
    if (is.na(x)) return(NA_integer_)
    if (abs(x - round(x)) < 1e-9 || abs(x) >= 1000) return(0L)  # inteiros / milhares
    ax <- abs(x)
    if (ax >= 1) return(1L)
    if (ax >= 0.1) {
      return(if (abs(round(x, 1) - x) < 1e-9) 1L else 2L)
    }
    if (ax >= 0.01) {
      return(if (abs(round(x, 2) - x) < 1e-9) 2L else 3L)
    }
    return(3L)
  }

  fmt_vec <- function(v) {
    dec <- vapply(v, decimals_for_one, integer(1))
    out <- character(length(v))
    for (i in seq_along(v)) {
      if (is.na(v[i])) { out[i] <- NA_character_; next }
      # accuracy coerente com o numero de casas; fallback para 0.001
      acc <- if (is.na(dec[i])) 0.001 else if (dec[i] == 0L) 1 else 10^(-dec[i])
      out[i] <- scales::number(v[i], accuracy = acc, big.mark = big_mark,
                               decimal.mark = decimal_mark, trim = TRUE)
    }
    out
  }

  agg <- tb |>
    dplyr::mutate(viol = !is.na(status) & status != "ok") |>
    dplyr::group_by(parametro) |>
    dplyr::summarise(
      n      = dplyr::n(),
      n_viol = sum(viol, na.rm = TRUE),
      idx    = { w <- which(viol); if (length(w)) w[which.max(abs(delta[w]))] else NA_integer_ },
      v  = ifelse(is.na(idx), NA_real_, valor[idx]),
      mn = ifelse(is.na(idx), NA_real_, lim_min[idx]),
      mx = ifelse(is.na(idx), NA_real_, lim_max[idx]),
      dl = ifelse(is.na(idx), NA_real_, delta[idx]),
      .groups = "drop"
    )

  if (only_violations) agg <- agg[agg$n_viol > 0, , drop = FALSE]

  v_txt  <- fmt_vec(agg$v)
  mn_txt <- fmt_vec(agg$mn)
  mx_txt <- fmt_vec(agg$mx)
  dl_txt <- fmt_vec(agg$dl)

  lim_min_txt <- ifelse(is.na(agg$mn), "", paste0("lim_min=", mn_txt))
  lim_max_txt <- ifelse(is.na(agg$mx), "", paste0("lim_max=", mx_txt))
  lim_comb0   <- trimws(paste(lim_min_txt, lim_max_txt))
  lim_comb    <- ifelse(nzchar(lim_comb0), paste0(lim_comb0, " "), "")

  sign <- ifelse(agg$dl > 0, "+", "")
  exemplos <- ifelse(
    is.na(agg$v) | agg$n_viol == 0,
    "-",
    paste0("ex.: valor=", v_txt, " (", lim_comb, "delta=", sign, dl_txt, ")")
  )

  linhas <- sprintf(
    "- %s: %d/%d violacoes%s",
    agg$parametro, agg$n_viol, agg$n,
    ifelse(exemplos == "-", "", paste0(" (", exemplos, ")"))
  )

  c(
    sprintf("Conformidade CONAMA classe %s:", classe),
    if (!nrow(agg)) "- todos os parametros avaliados estao conformes." else linhas
  )
}
#' Relatorio de conformidade (tabela), com formato opcional
#' @param df dados de entrada
#' @param classe classe CONAMA
#' @param only_violations se TRUE, retorna apenas linhas com status != "ok"
#' @param pretty se TRUE, devolve colunas formatadas para exibicao
#' @param decimal_mark separador decimal (default ",")
#' @param big_mark separador de milhar (default ".")
#' @return tibble
#' @export
conama_report <- function(df, classe = "2",
                          only_violations = TRUE,
                          pretty = FALSE,
                          decimal_mark = ",", big_mark = ".") {

  tb <- conama_summary(df, classe = classe)

  if (only_violations) {
    tb <- tb[!is.na(tb$status) & tb$status != "ok", , drop = FALSE]
  }
  tb <- tb[order(tb$parametro), ]

  if (!pretty) return(tb)

  # ---- formatacao numerica "natural" (ASCII-only) ---------------------------
  decimals_for_one <- function(x) {
    if (is.na(x)) return(NA_integer_)
    if (abs(x - round(x)) < 1e-9 || abs(x) >= 1000) return(0L)  # inteiros/milhares
    ax <- abs(x)
    if (ax >= 1) return(1L)
    if (ax >= 0.1) return(if (abs(round(x, 1) - x) < 1e-9) 1L else 2L)
    if (ax >= 0.01) return(if (abs(round(x, 2) - x) < 1e-9) 2L else 3L)
    3L
  }
  fmt_vec <- function(v) {
    dec <- vapply(v, decimals_for_one, integer(1))
    acc <- ifelse(is.na(dec) | dec == 0L, 1, 10^(-dec))
    out <- character(length(v))
    for (i in seq_along(v)) {
      if (is.na(v[i])) { out[i] <- NA_character_; next }
      out[i] <- scales::number(v[i], accuracy = acc[i],
                               big.mark = big_mark, decimal.mark = decimal_mark,
                               trim = TRUE)
    }
    out
  }

  dplyr::tibble(
    parametro = tb$parametro,
    valor     = fmt_vec(tb$valor),
    lim_min   = fmt_vec(tb$lim_min),
    lim_max   = fmt_vec(tb$lim_max),
    status    = tb$status,
    delta     = paste0(ifelse(tb$delta > 0, "+", ""), fmt_vec(tb$delta))
  )
}

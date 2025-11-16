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
#' @returns A tibble/data frame with one row per parameter and regulatory thresholds.
#'   Typical columns:
#'   \itemize{
#'     \item \code{parametro}: parameter name (character, normalized to snake_case)
#'     \item \code{classe}: class label (character)
#'     \item \code{min}/\code{max} (or equivalents): numeric thresholds (may be \code{NA})
#'     \item other metadata columns if present (e.g., unit, criterion)
#'   }
#'
#' @examples
#' # Class 2 thresholds (first rows)
#' head(conama_limits(2))
#'
#' @export
conama_limits <- function(class){
  path <- system.file("extdata","conama_limits.csv", package = "tikatuwq")
  df <- readr::read_csv(path, show_col_types = FALSE, na = c("", "NA"))
  df$classe <- as.character(df$classe)
  # normalize parameter names to match data columns
  df$parametro <- df$parametro |>
    tolower() |>
    stringr::str_replace_all("\\s+", "_") |>
    stringr::str_replace_all("[^a-z0-9_]", "")
  if (missing(class)) return(df)
  # filter by class (accepts numeric/character)
  df[as.character(df$classe) == as.character(class), , drop = FALSE]
}

# Internal helpers -------------------------------------------------------------

# extract min/max from a limit row (supports multiple column name variants)
.get_minmax <- function(row) {
  nms <- names(row)
  minv <- NA_real_
  maxv <- NA_real_
  mmn <- intersect(c("min", "minimo", "lim_min"), nms)
  mxn <- intersect(c("max", "maximo", "lim_max"), nms)
  if (length(mmn)) minv <- suppressWarnings(as.numeric(row[[mmn[1]]]))
  if (length(mxn)) maxv <- suppressWarnings(as.numeric(row[[mxn[1]]]))
  list(min = minv, max = maxv)
}

# status/delta given a value and min/max
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

#' CONAMA conformity check (detailed; default class = "2")
#'
#' @description For each parameter present in \code{df}, adds columns:
#' \itemize{
#'   \item \code{*_ok} (logical),
#'   \item \code{*_status} one of \code{"ok"}, \code{"acima_do_maximo"}, \code{"abaixo_do_minimo"},
#'   \item \code{*__lim_min} and \code{*__lim_max} (thresholds used),
#'   \item \code{*__delta} (difference to the relevant limit; >0 above max, <0 below min, 0 if ok).
#' }
#' If multiple limit rows exist for the same parameter, \code{*_ok} is TRUE if
#' any row is satisfied; for \code{status/lim_min/lim_max/delta}, the first
#' satisfied row is chosen; if none satisfy, the row with the smallest
#' absolute violation (min |delta|) is used.
#'
#' @param df A tibble/data.frame with parameter columns (e.g., ph, turbidez, od, dbo).
#' @param classe Character class label (e.g., "especial", "1", "2", "3", "4").
#'
#' @returns The input \code{df} with additional columns per parameter as described.
#'
#' @seealso \code{\link[=conama_limits]{conama_limits()}}, \code{\link[=conama_summary]{conama_summary()}}, \code{\link[=conama_report]{conama_report()}}, \code{\link[=conama_text]{conama_text()}}
#'
#' @examples
#' \dontrun{
#' data("wq_demo", package = "tikatuwq")
#' head(conama_check(wq_demo, classe = "2"))
#' }
#'
#' @export
#' @importFrom rlang .data
conama_check <- function(df, classe = "2") {
  lim_all <- conama_limits()
  lim <- lim_all[as.character(lim_all$classe) == as.character(classe), , drop = FALSE]
  if (!nrow(lim)) return(df)

  out <- df
  params_present <- intersect(unique(lim$parametro), names(df))
  if (!length(params_present)) return(out)

  for (p in params_present) {
    v <- out[[p]]

    # coerce to numeric if needed
    if (!is.numeric(v)) {
      # Try direct conversion first
      v_num <- suppressWarnings(as.numeric(v))
      # If all NA, try helper from io_clean.R if available in namespace
      if (all(is.na(v_num))) {
        # Use internal helper from tikatuwq namespace if available
        to_num_helper <- tryCatch(
          get(".to_number_auto", envir = asNamespace("tikatuwq"), inherits = FALSE),
          error = function(e) NULL
        )
        if (!is.null(to_num_helper)) {
          v_num <- to_num_helper(v)
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

      # row-wise satisfaction
      ok_j <- (is.na(minv) | (!is.na(v) & v >= minv)) &
              (is.na(maxv) | (!is.na(v) & v <= maxv))
      ok_mat[, j] <- ok_j

      # status/delta per row
      for (i in seq_len(n)) {
        sd <- .status_delta_one(v[i], minv, maxv)
        status_m[i, j] <- sd$status
        delta_m[i, j]  <- sd$delta
        if (ok_j[i]) delta_m[i, j] <- 0
      }
    }

    # combine rows for each observation
    ok_any <- apply(ok_mat, 1, any)
    first_ok_idx <- apply(ok_mat, 1, function(x) { w <- which(x); if (length(w)) w[1] else NA_integer_ })

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
        deltas <- delta_m[i, ]
        if (all(is.na(deltas))) {
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

#' CONAMA conformity summary (long format)
#'
#' @param df Input data
#' @param classe CONAMA class label
#'
#' @returns A tibble with columns:
#'   \code{parametro}, \code{valor}, \code{lim_min}, \code{lim_max},
#'   \code{status}, \code{ok}, \code{delta}.
#'
#' @seealso \code{\link[=conama_check]{conama_check()}}, \code{\link[=conama_report]{conama_report()}}, \code{\link[=conama_text]{conama_text()}}
#'
#' @examples
#' \dontrun{
#' data("wq_demo", package = "tikatuwq")
#' head(conama_summary(wq_demo, classe = "2"))
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom purrr map_dfr
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

#' CONAMA conformity report (table)
#'
#' @param df Input data
#' @param classe CONAMA class label (e.g., "2")
#' @param only_violations If TRUE, returns only rows with \code{status != "ok"}
#' @param pretty If TRUE, returns formatted numeric columns for display
#' @param decimal_mark Decimal separator (default \code{","})
#' @param big_mark Thousands separator (default \code{"."})
#'
#' @returns A tibble. When \code{pretty = FALSE}:
#'   \code{parametro}, \code{valor}, \code{lim_min}, \code{lim_max},
#'   \code{status}, \code{delta}. When \code{pretty = TRUE}, numeric columns
#'   are formatted as character with "natural" decimals.
#'
#' @seealso \code{\link[=conama_summary]{conama_summary()}}, \code{\link[=conama_text]{conama_text()}}
#'
#' @examples
#' \dontrun{
#' data("wq_demo", package = "tikatuwq")
#' conama_report(wq_demo, classe = "2", only_violations = TRUE)
#' conama_report(wq_demo, classe = "2", only_violations = TRUE, pretty = TRUE)
#' }
#'
#' @export
#' @importFrom rlang .data
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

  # numeric formatting "natural" (ASCII-only)
  decimals_for_one <- function(x) {
    if (is.na(x)) return(NA_integer_)
    if (abs(x - round(x)) < 1e-9 || abs(x) >= 1000) return(0L)
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

#' Text summary of conformity (bulleted, formatted)
#'
#' @param df Input data
#' @param classe CONAMA class label
#' @param only_violations If TRUE, list only parameters with violation
#' @param decimal_mark Decimal separator (default \code{","})
#' @param big_mark Thousands separator (default \code{"."})
#'
#' @returns Character vector of lines (first line is a header, the rest are bullets).
#'
#' @seealso \code{\link[=conama_summary]{conama_summary()}}, \code{\link[=conama_report]{conama_report()}}
#'
#' @examples
#' \dontrun{
#' data("wq_demo", package = "tikatuwq")
#' cat(conama_text(wq_demo, classe = "2"), sep = "\n")
#' }
#'
#' @export
#' @importFrom rlang .data
conama_text <- function(df, classe = "2",
                        only_violations = FALSE,
                        decimal_mark = ",", big_mark = ".") {
  tb <- conama_summary(df, classe = classe)
  if (!nrow(tb)) return("Sem parametros aplicaveis para a classe informada.")
  tb <- dplyr::arrange(tb, .data$parametro)

  # "natural" decimal places: integers without ,0; 0.1 -> 0,1; 0.05 -> 0,05; 0.123 -> 0,123
  decimals_for_one <- function(x) {
    if (is.na(x)) return(NA_integer_)
    if (abs(x - round(x)) < 1e-9 || abs(x) >= 1000) return(0L)
    ax <- abs(x)
    if (ax >= 1) return(1L)
    if (ax >= 0.1)  return(if (abs(round(x, 1) - x) < 1e-9) 1L else 2L)
    if (ax >= 0.01) return(if (abs(round(x, 2) - x) < 1e-9) 2L else 3L)
    3L
  }

  fmt_vec <- function(v) {
    dec <- vapply(v, decimals_for_one, integer(1))
    out <- character(length(v))
    for (i in seq_along(v)) {
      if (is.na(v[i])) { out[i] <- NA_character_; next }
      acc <- if (is.na(dec[i])) 0.001 else if (dec[i] == 0L) 1 else 10^(-dec[i])
      out[i] <- scales::number(v[i], accuracy = acc, big.mark = big_mark,
                               decimal.mark = decimal_mark, trim = TRUE)
    }
    out
  }

  agg <- tb |>
    dplyr::mutate(viol = !is.na(.data$status) & .data$status != "ok") |>
    dplyr::group_by(.data$parametro) |>
    dplyr::summarise(
      n      = dplyr::n(),
      n_viol = sum(.data$viol, na.rm = TRUE),
      idx    = { w <- which(.data$viol); if (length(w)) w[which.max(abs(.data$delta[w]))] else NA_integer_ },
      v  = ifelse(is.na(.data$idx), NA_real_, .data$valor[.data$idx]),
      mn = ifelse(is.na(.data$idx), NA_real_, .data$lim_min[.data$idx]),
      mx = ifelse(is.na(.data$idx), NA_real_, .data$lim_max[.data$idx]),
      dl = ifelse(is.na(.data$idx), NA_real_, .data$delta[.data$idx]),
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

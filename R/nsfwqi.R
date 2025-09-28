# R/nsfwqi.R
# NSF Water Quality Index (prototype) — ASCII-only

#' NSF Water Quality Index (NSF WQI, prototype)
#'
#' @description
#' Computes a **prototype** NSF WQI as a weighted arithmetic mean of
#' parameter sub-scores (Qi) using simple piecewise rules. This is intended
#' for quick demonstrations and is **not** a full replication of the original
#' NSF curves.
#'
#' @details
#' The function accepts both NSF-style column names and common Brazilian
#' aliases. The mapping tried (if present) is:
#'
#' - \code{do}  <- \code{od}
#' - \code{fc}  <- \code{coliformes}
#' - \code{ph}  <- \code{pH} or \code{ph}
#' - \code{bod} <- \code{dbo}
#' - \code{turbidez} stays \code{turbidez}
#' - \code{sst} <- \code{solidos_suspensos}
#' - \code{po4} <- \code{po4} or \code{p_ortofosfato}
#' - \code{no3} <- \code{no3} or \code{n_nitrato}
#' - \code{temp_change} must be supplied as-is (delta T to reference)
#'
#' If \code{na_rm = TRUE}, weights are rescaled **per row** to the parameters
#' available in that row. If \code{na_rm = FALSE} (default), any missing
#' required input leads to an error.
#'
#' @param df Data frame containing columns compatible with the mapping above.
#' @param pesos Named numeric vector with parameter weights. Defaults follow a
#'   common NSF WQI variant:
#'   \code{do=.17}, \code{fc=.16}, \code{ph=.11}, \code{bod=.11},
#'   \code{temp_change=.10}, \code{po4=.10}, \code{no3=.10},
#'   \code{turbidez=.08}, \code{sst=.07}.
#' @param na_rm Logical; allow NA per row and rescale weights to available
#'   parameters (\code{TRUE}) or error on missing inputs (\code{FALSE}).
#'
#' @return The input \code{df} with an added numeric column \code{NSFWQI}.
#'
#' @examples
#' d <- wq_demo
#' # create minimal aliases so the prototype can run
#' d$do  <- d$od
#' d$fc  <- d$coliformes
#' d$ph  <- d$ph
#' d$bod <- d$dbo
#' # others are missing; use na_rm = TRUE to rescale weights by row
#' out <- nsfwqi(d, na_rm = TRUE)
#' head(out$NSFWQI)
#'
#' @export
nsfwqi <- function(
  df,
  pesos = c(
    do = 0.17, fc = 0.16, ph = 0.11, bod = 0.11, temp_change = 0.10,
    po4 = 0.10, no3 = 0.10, turbidez = 0.08, sst = 0.07
  ),
  na_rm = FALSE
) {

  # ---- column aliasing (BR -> NSF) -----------------------------------------
  col_present <- function(nm) nm %in% names(df)

  # build a working view with NSF names where possible
  work <- list()

  # primary names first; if missing, try aliases
  if (col_present("do"))  work$do  <- df$do  else if (col_present("od")) work$do <- df$od
  if (col_present("fc"))  work$fc  <- df$fc  else if (col_present("coliformes")) work$fc <- df$coliformes
  if (col_present("ph"))  work$ph  <- df$ph  else if (col_present("pH")) work$ph <- df$`pH` else if (col_present("ph")) work$ph <- df$ph
  if (col_present("bod")) work$bod <- df$bod else if (col_present("dbo")) work$bod <- df$dbo

  if (col_present("turbidez")) work$turbidez <- df$turbidez

  if (col_present("sst")) work$sst <- df$sst else if (col_present("solidos_suspensos")) work$sst <- df$solidos_suspensos

  if (col_present("po4")) work$po4 <- df$po4 else if (col_present("p_ortofosfato")) work$po4 <- df$p_ortofosfato

  if (col_present("no3")) work$no3 <- df$no3 else if (col_present("n_nitrato")) work$no3 <- df$n_nitrato

  if (col_present("temp_change")) work$temp_change <- df$temp_change
  # no automatic derivation for temp_change (needs reference)

  # assemble working data.frame
  if (length(work)) {
    wdf <- as.data.frame(work, stringsAsFactors = FALSE)
  } else {
    stop("No compatible columns found for NSFWQI.")
  }

  # select weights for available columns
  use <- intersect(names(pesos), names(wdf))
  if (!length(use)) {
    stop("No available columns among: ", paste(names(pesos), collapse = ", "))
  }
  wdf <- wdf[use, drop = FALSE]
  w_full <- pesos[use]

  # if na_rm = FALSE, check for any NA in used columns and fail early
  if (!na_rm) {
    if (any(is.na(wdf))) {
      stop("Missing values in required parameters. Set na_rm = TRUE to rescale weights per row.")
    }
  }

  # ---- Qi functions (simple piecewise placeholders) ------------------------
  qi_piecewise <- function(param, v) {
    if (is.na(v)) return(NA_real_)
    switch(
      param,
      "do"          = ifelse(v >= 7.5, 90, ifelse(v >= 6, 80, ifelse(v >= 5, 70, 50))),
      "fc"          = ifelse(v <= 200, 90, ifelse(v <= 1000, 70, 40)),
      "ph"          = ifelse(v >= 6.5 & v <= 8.5, 90, 60),
      "bod"         = ifelse(v <= 3, 90, ifelse(v <= 5, 75, 55)),
      "temp_change" = ifelse(v <= 2, 90, ifelse(v <= 5, 75, 55)),
      "po4"         = ifelse(v <= 0.05, 90, ifelse(v <= 0.10, 75, 55)),
      "no3"         = ifelse(v <= 1, 90, ifelse(v <= 10, 70, 50)),
      "turbidez"    = ifelse(v <= 5, 90, ifelse(v <= 50, 70, 50)),
      "sst"         = ifelse(v <= 500, 85, 60),
      50
    )
  }

  # compute Qi matrix (same column order as 'use')
  qi_mat <- matrix(NA_real_, nrow = nrow(wdf), ncol = length(use))
  for (j in seq_along(use)) {
    p <- use[j]
    qi_mat[, j] <- vapply(wdf[[p]], function(x) qi_piecewise(p, x), numeric(1))
  }

  # row-wise weighted mean with optional NA-removal and weight rescaling
  NSFWQI <- rep(NA_real_, nrow(wdf))
  base_w_sum <- sum(w_full)

  for (i in seq_len(nrow(wdf))) {
    row_qi <- qi_mat[i, ]
    ok <- is.finite(row_qi)
    if (!any(ok)) {
      NSFWQI[i] <- NA_real_
      next
    }
    if (na_rm) {
      w_i <- w_full[ok]
      w_i <- w_i / sum(w_i)
      NSFWQI[i] <- sum(row_qi[ok] * w_i)
    } else {
      # all must be present if na_rm = FALSE (already checked for NA globally)
      NSFWQI[i] <- sum(row_qi * (w_full / base_w_sum))
    }
  }

  # attach to original df (preserving rows)
  out <- df
  out$NSFWQI <- NSFWQI
  out
}

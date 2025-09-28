# R/summaries.R
# ASCII-only in code/comments.

#' Descriptive summaries by group
#'
#' @description
#' Computes basic descriptive statistics (mean, median, sd) for all **numeric**
#' columns in `df`, grouped by one or more keys.
#'
#' @details
#' - Grouping columns not found in `df` are silently dropped.
#' - If no grouping columns remain, an error is thrown.
#' - Only numeric columns are summarized; if none exist, an error is thrown.
#' - Missing values are ignored (`na.rm = TRUE`).
#'
#' @param df A data frame or tibble.
#' @param by Character vector with grouping column names (default `c("ponto","mes")`).
#'   Any names not present in `df` are ignored.
#' @param funs **Deprecated** (kept for compatibility; ignored). The function
#'   always computes `mean`, `median` and `sd` with `na.rm = TRUE`.
#'
#' @return A tibble with the grouping keys and one column per
#'   statistic/variable, named as `{var}_{stat}` (e.g., `od_mean`, `od_median`, `od_sd`).
#'
#' @examples
#' \donttest{
#' # Using the demo dataset shipped with the package
#' d <- wq_demo
#' # Example: group by point (ponto)
#' s1 <- resume_wq(d, by = "ponto")
#' head(s1)
#'
#' # Example: group by point and month (if 'mes' exists in your data)
#' # s2 <- resume_wq(d, by = c("ponto", "mes"))
#' }
#'
#' @seealso [dplyr::summarise()], [dplyr::across()]
#' @export
#' @importFrom dplyr group_by summarise across all_of
resume_wq <- function(df,
                      by = c("ponto", "mes"),
                      funs = c("mean", "median", "sd")) {

  # Drop grouping names not present
  by <- intersect(by, names(df))
  if (!length(by)) stop("Grouping columns not found in 'df' (after intersect).")

  # Numeric columns to summarize
  numcols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (!length(numcols)) stop("No numeric columns to summarize in 'df'.")

  # Summaries with na.rm = TRUE and stable names {var}_{stat}
  dplyr::group_by(df, dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(numcols),
        list(
          mean   = ~ mean(.x, na.rm = TRUE),
          median = ~ stats::median(.x, na.rm = TRUE),
          sd     = ~ stats::sd(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
}

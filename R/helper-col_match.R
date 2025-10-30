#' Robust column matcher tolerant to R name-mangling
#'
#' @description
#' Return the subset of `colnames(obj)` that correspond to `want`, treating
#' `"Stock C"` and `"Stock.C"` as equivalent. The result preserves the order of `want`
#' and drops non-matches. Column names in `obj` are **not** modified.
#'
#' @param obj A matrix-, data.frame-, or xts-like object with `base::colnames()`.
#' @param want Character vector of desired column labels (may be non-syntactic).
#' @param ignore_case Logical; if `TRUE`, matching is case-insensitive. Default `FALSE`.
#'
#' @return Character vector of column names from `obj` that match `want`,
#'         in the same order as `want`. Non-matches are omitted.
#'
#' @details
#' Matching is performed on `base::make.names(x, unique = FALSE)`. This mirrors
#' how R sanitizes names (spaces/dashes → dots; leading digits prefixed with `X`).
#' If multiple columns in `obj` normalize to the same token, the first occurrence
#' is selected (per `base::match()` semantics).
#'
#' @examples
#' cn <- c("Stock A", "Stock.B", "X1Price")
#' X  <- stats::setNames(matrix(0, 2, 3), cn)
#' col_match(X, c("Stock A", "Stock B", "1Price"))
#' # -> c("Stock A", "Stock.B", "X1Price")
#'
#' # Case-insensitive match:
#' col_match(X, c("stock a", "stock b"), ignore_case = TRUE)
#'
#' @seealso base::make.names, base::match
#' @keywords internal
col_match <- function(obj, want, ignore_case = FALSE) {
  cn <- base::colnames(obj)
  if (is.null(cn) || length(cn) == 0L || length(want) == 0L) return(character(0L))

  normalise <- function(x) {
    y <- base::make.names(x, unique = FALSE)
    if (ignore_case) base::tolower(y) else y
  }

  idx <- base::match(normalise(want), normalise(cn), nomatch = 0L)
  cn[idx[idx > 0L]]
}

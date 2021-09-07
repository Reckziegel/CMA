#' @keywords internal
assert_is_multivariate <- function(x) {
  assertthat::assert_that(
    NCOL(x) > 1,
    msg = "The `x` argument must be multivariate."
    )
}

#' @keywords internal
assert_is_univariate <- function(x) {
  assertthat::assert_that(
    NCOL(x) == 1,
    msg = "The `x` argument must be univariate."
  )
}

#' @keywords internal
assert_is_prob <- function(p) {
  assertthat::assert_that(is.double(p), msg = "`p` must be a double vector.")
  assertthat::assert_that(abs(sum(p) - 1) < 0.001, msg = "`p` must sum up to 1.")
  assertthat::assert_that(all(p > 0), msg = "`p` must contain only positive values.")
}

#' @keywords internal
assert_cols_length <- function(x, y) {
  assertthat::assert_that(assertthat::are_equal(NCOL(x), NCOL(y)),
                          msg = "Objects must have the same number of columns.")
}

#' @keywords internal
assert_rows_length <- function(x, y) {
  assertthat::assert_that(assertthat::are_equal(NROW(x), NROW(y)),
                          msg = "Objects must have the same number of rows.")
}


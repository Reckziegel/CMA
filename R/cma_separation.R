#' CMA Separation Step
#'
#' This function decomposes the the pure "individual" features in the
#' marginal distributions from the pure "joint" information available in the copulas.
#'
#' Arguments \code{x} and \code{p} must have the same size.
#'
#' @param x A rectangular (non-tidy) data structure.
#' @param p A probability vector.
#'
#' @return An S3 list of the \code{cma} class that contains two elements:
#' \code{marginal} and \code{copula}.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' margins <- matrix(stats::rnorm(20), ncol = 2)
#' probs   <- rep(1 / 20, 20)
#' cma_separation(x = margins, p = probs)
cma_separation <- function(x, p = NULL) {
  UseMethod("cma_separation", x)
}

#' @rdname cma_separation
#' @export
cma_separation.default <- function(x, p = NULL) {
  stop(paste0("CMA doesn't support the `", class(x), "` class yet.", call. = FALSE))
}

#' @rdname cma_separation
#' @export
cma_separation.matrix <- function(x, p = NULL) {

  assert_is_multivariate(x)
  if (is_empty(p)) {
    p <- rep(1 / NROW(x), NROW(x))
  } else {
    assert_is_prob(p)
  }

  new_cma_separation(x, cma_separate(x = x, p = p))

}

#' @rdname cma_separation
#' @export
cma_separation.xts <- function(x, p = NULL) {

  assert_is_multivariate(x)
  if (is_empty(p)) {
    p <- rep(1 / NROW(x), NROW(x))
  } else {
    assert_is_prob(p)
  }
  x <- sanitize(x)

  new_cma_separation(x, cma_separate(x = x, p = p))

}

# #' @rdname cma_separation
# #' @export
# cma_separation.ts <- function(x, p = NULL) {
#
#   assert_is_multivariate(x)
#   if (is_empty(p)) {
#     p <- rep(1 / NROW(x), NROW(x))
#   } else {
#     assert_is_prob(p)
#   }
#   x <- sanitize(x)
#
#   new_cma_separation(x, cma_separate(x = x, p = p))
#
# }

#' @rdname cma_separation
#' @export
cma_separation.data.frame <- function(x, p = NULL) {

  index <- dplyr::if_else(any_is_date(x), get_date_col(x), NULL)
  x <- get_double_col(x)

  assert_is_multivariate(x)
  assert_is_prob(p)

  names <- names(x)
  x <- as.matrix(x)

  new_cma_separation(x, cma_separate(x = x, p = p))

}

#' @rdname cma_separation
#' @export
cma_separation.tbl <- function(x, p = NULL) {
  assert_is_multivariate(x)
  assert_is_prob(p)
  x <- matrix(x, nrow = NROW(x), ncol = NCOL(x))
  new_cma_separation(x, cma_separate(x = x, p = p))
}




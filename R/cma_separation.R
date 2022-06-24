#' CMA Separation Step
#'
#' First CMA step: decomposes the the pure "individual" features in the marginals
#' from the pure "joint" information available in the copulas.
#'
#' Arguments \code{x} and \code{p} must have the same size.
#'
#' @param x A rectangular (non-tidy) data structure.
#' @param p A probability vector. If \code{NULL} (the default) the standard
#' 1 over N is used.
#'
#' @return An S3 list of the \code{cma} class that contains three elements:
#' \code{marginal}, \code{cdf} and \code{copula}.
#'
#' @export
#'
#' @seealso \code{\link{cma_combination}}
#'
#' @examples
#' set.seed(123)
#'
#' margins <- matrix(stats::rnorm(30), ncol = 3)
#' colnames(margins) <- c("a", "b", "c")
#'
#' cma <- cma_separation(x = margins)
#' cma
#'
#' # Access the elements with `$`
#' cma$copula
#' cma$marginal
#' cma$cdf
cma_separation <- function(x, p = NULL) {
  UseMethod("cma_separation", x)
}

#' @rdname cma_separation
#' @export
cma_separation.default <- function(x, p = NULL) {
  rlang::abort("CMA doesn't support the `", class(x), "` class yet.")
}

#' @rdname cma_separation
#' @export
cma_separation.matrix <- function(x, p = NULL) {

  x <- check_input(x)
  assert_is_multivariate(x)
  if (is.null(p)) {
    p <- rep(1 / NROW(x), NROW(x))
  } else {
    p <- check_p(p)
  }

  new_cma_separation(x, cma_separate(x = x, p = p))

}

#' @rdname cma_separation
#' @export
cma_separation.xts <- function(x, p = NULL) {

  x <- check_input(x)
  assert_is_multivariate(x)
  if (is.null(p)) {
    p <- rep(1 / NROW(x), NROW(x))
  } else {
    p <- check_p(p)
  }

  new_cma_separation(x, cma_separate(x = x, p = p))

}

#' @rdname cma_separation
#' @export
cma_separation.data.frame <- function(x, p = NULL) {

  x <- check_input(x)
  assert_is_multivariate(x)
  if (is.null(p)) {
    p <- rep(1 / NROW(x), NROW(x))
  } else {
    p <- check_p(p)
  }

  new_cma_separation(x, cma_separate(x = x, p = p))

}

#' @rdname cma_separation
#' @export
cma_separation.tbl <- function(x, p = NULL) {

  x <- check_input(x)
  assert_is_multivariate(x)
  if (is.null(p)) {
    p <- rep(1 / NROW(x), NROW(x))
  } else {
    p <- check_p(p)
  }

  new_cma_separation(x, cma_separate(x = x, p = p))

}


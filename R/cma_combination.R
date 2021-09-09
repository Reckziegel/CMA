#' CMA combination Step
#'
#' This function glues back any arbitrary copula to marginal distributions
#' of any kind.
#'
#' @param x A rectangular (non-tidy) data structure with the sorted marginals
#' (ascending order).#' .
#' @param u A rectangular (non-tidy) data structure with the CDF's.
#' @param U A rectangular (non-tidy) data structure with the copulas.
#'
#' @return A \code{tbl} with the joint scenarios.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#'
#' margins <- matrix(stats::rnorm(20), ncol = 2)
#' colnames(margins) <- c("a", "b")
#' probs   <- rep(1 / 25, 25)
#'
#' # separate
#' sep <- cma_separation(x = margins, p = probs)
#' # combinate
#' comb <- cma_combination(x = attributes(sep)$ord_margin, u = attributes(sep)$cdf, U = sep$copula)
#'
#' # The result is identical
#' stopifnot(all.equal(margins, comb))
cma_combination <- function(x, u, U) {

  #assert_cols_length(x, u)
  #assert_cols_length(x, U)
  #assert_cols_length(u, U)
  #assert_rows_length(x, u)
  #assert_rows_length(x, U)
  #assert_rows_length(u, U)

  x <- check_input(x)
  u <- check_input(u)
  U <- check_input(U)

  cma_combine(x = x, u = u, U = U)

}

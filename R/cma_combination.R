#' CMA combination Step
#'
#' Joins copulas and marginals into the same dataset.
#'
#' @param x A rectangular (non-tidy) data structure with the sorted marginals
#' (ascending order).
#' @param cdf A rectangular (non-tidy) data structure with the CDF's.
#' @param copula A rectangular (non-tidy) data structure with the copulas.
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
#' comb <- cma_combination(attributes(sep)$ord_margin, attributes(sep)$cdf, sep$copula)
#'
#' # The result is identical
#' stopifnot(all.equal(margins, comb))
cma_combination <- function(x, cdf, copula) {

  #assert_cols_length(x, u)
  #assert_cols_length(x, U)
  #assert_cols_length(u, U)
  #assert_rows_length(x, u)
  #assert_rows_length(x, U)
  #assert_rows_length(u, U)

  x <- check_input(x)
  u <- check_input(cdf)
  U <- check_input(copula)

  cma_combine(x = x, u = cdf, U = copula)

}

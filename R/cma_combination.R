#' CMA combination Step
#'
#' Second CMA step: "glue" copulas and marginals into the same data structure.
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
#' @seealso \code{\link{cma_separation}}
#'
#' @examples
#' set.seed(123)
#'
#' margins <- matrix(stats::rnorm(20), ncol = 2)
#' colnames(margins) <- c("a", "b")
#' probs   <- rep(1 / 10, 10)
#'
#' # separate
#' sep <- cma_separation(x = margins, p = probs)
#' # combinate
#' comb <- cma_combination(margins, sep$cdf, sep$copula)
#'
#' # The result is identical
#' margins
#' comb
#' stopifnot(all.equal(margins[ , "a"], comb$a))
#' stopifnot(all.equal(margins[ , "b"], comb$b))
cma_combination <- function(x, cdf, copula) {

  #assert_cols_length(x, u)
  #assert_cols_length(x, U)
  #assert_cols_length(u, U)
  #assert_rows_length(x, u)
  #assert_rows_length(x, U)
  #assert_rows_length(u, U)

  x      <- check_input(x)
  cdf    <- check_input(cdf)
  copula <- check_input(copula)

  cma_combine(x = x, u = cdf, U = copula)

}

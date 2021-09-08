#' Simulations with Exact Means and Covariances
#'
#' Generates scenarios from elliptical distributions in which the
#' sample moments match the populational moments.
#'
#' @param M A \code{1xN} matrix with the location parameter of each invariant (asset).
#' @param S A \code{NxN} matrix with the dispersion matrix of the invariants (assets).
#' @param J A numeric scalar with the desired number of scenarios.
#'
#' @return A J x N matrix with the simulated time-series.
#'
#' @export
#'
#' @examples
#' rets  <- diff(log(EuStockMarkets))
#' mu    <- colMeans(rets)
#' sigma <- stats::cov(rets)
#' match_moments_normal(M = mu, S = sigma, J = 10)
match_moments_normal <- function(M, S, J) {

  assertthat::assert_that(assertthat::is.number(J))

  if (!is.matrix(M)) {
    M <- t(as.matrix(M))
  }

  if (nrow(M) == ncol(S)) {
    M <- t(M)
  }

  N <- length(M)

  # generate antithetic variables (mean = 0)
  Y <- MASS::mvrnorm(n = J / 2, mu = matrix(0, N, 1), Sigma = S)
  Y <- rbind(Y, -Y)

  # compute sample covariance: NOTE defined as "cov(Y,1)", not as "cov(Y)"
  S_ <- (nrow(Y) - 1) / nrow(Y) * stats::cov(Y)

  # solve Riccati equation using Schur method
  H <- rbind(
    cbind(matrix(0, N, N), -S_),
    cbind(-S, matrix(0, N, N))
  )

  U <- QZ::ordqz(H, keyword = "lhp")$Q

  U_lu <- U[1:N, 1:N]
  U_ld <- U[(N + 1):nrow(U), 1:N]

  B <- U_ld %*% solve(U_lu)

  # affine transformation to match mean and covariances
  X <- Y %*% B + matlab::repmat(t(M), J, 1)

  X

}

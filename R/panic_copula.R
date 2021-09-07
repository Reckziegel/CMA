#' Panic-Copula for Stress-Testing
#'
#' This function generates a large dimensional panic copula that can be further
#' used for stress-testing and panic-aware portfolio optimization.
#'
#' @param N A numeric value with the number of assets.
#' @param J A numeric value with the number of scenarios to be generated.
#' @param calm_cor A numeric value for the correlation in calm markets.
#' @param panic_cor A numeric value for the correlation in panic markets.
#' @param panic_prob A numeric value with the probability in which panic markets
#' can be triggered.
#' @param sigma The unconditional volatility of the joint-distribution.
#'
#' @return An object with \code{N} columns and \code{J} rows.
#'
#' @export
#'
#' @examples
#' panic_copula(N = 2, J = 10, calm_cor = 0.3, panic_cor = 0.99, panic_prob = 0.02, sigma = 0.2)
panic_copula <- function(N = 2, J = 50000, calm_cor = 0.3, panic_cor = .99, panic_prob = .02, sigma = 0.2) {

  assertthat::assert_that(assertthat::is.number(N))
  assertthat::assert_that(assertthat::is.number(J))
  assertthat::assert_that(assertthat::is.number(calm_cor))
  assertthat::assert_that(assertthat::is.number(panic_cor))
  assertthat::assert_that(assertthat::is.number(panic_prob))
  assertthat::assert_that(assertthat::is.number(sigma))

  # generate panic distribution
  p    <- matrix(1, nrow = J, ncol = 1) / J
  c2_c <- (1 - calm_cor) * diag(N) + calm_cor * matrix(1, N , N)
  c2_p <- (1 - panic_cor) * diag(N) + panic_cor * matrix(1, N, N)

  s2 <- pracma::blkdiag(c2_c, c2_p)
  Z <- match_moments_normal(matrix(0, 2 * N, 1), s2, J)

  X_c <- Z[ , 1:N]
  X_p <- Z[ , (N + 1):ncol(Z)]

  D <- stats::pnorm(X_p) < panic_prob

  X <- (1 - D) * X_c + (D * X_p)

  # perturb probabilities via Fully Flexible Views
  Aeq <- matrix(1, 1, J) # constrain probabilities to sum to one...
  Aeq <- rbind(Aeq , t(X)) # ...constrain the first moments...
  beq <- 1
  beq <- as.matrix(rbind(beq , matrix(0, N , 1)))
  p_  <- entropy_pooling(p, NULL, NULL, Aeq, beq)

  sep_step <- cma_separation(X, p_)

  # merge panic copula with normal marginals
  y <- NULL
  u <- NULL
  for (n in 1:N) {
    yn <- as.matrix(seq(from = -4 * sigma, to = 4 * sigma, length.out = 100))
    un <- stats::pnorm(yn , 0, sigma)

    if (is.null(y)) {
      y <- yn
      u <- un
    } else {
      y <- cbind(y, yn)
      u <- cbind(u, un)
    }
  }

  Y <- cma_combination(y, u, sep_step$copula)

  list(Y = Y, p_ = p_)

}


# Separation -------------------------------------------------------------

#' @keywords internal
cma_separate <- function(x, p) {

  J <- nrow(x)
  N <- ncol(x)
  l <- J / (J + 1)
  p <- apply(cbind(p, 1 / J * 10e-9), 1, max, na.rm = TRUE)
  p <- p / sum(p)
  u <- 0 * x
  U <- 0 * x

  # begin core algorithm
  # MATLAB does this operation in one shot. In R, we have to loop over...
  X    <- 0 * x
  Indx <- 0 * x
  for (n in 1:N) {
    tmp <- sort(x[ , n, drop = FALSE], index.return = TRUE)
    X[ , n]    <- tmp$x
    Indx[ , n] <- tmp$ix
  }

  # for each marginal...
  for (n in 1:N) {
    I       <- Indx[ , n]     # sort
    cum_p   <- cumsum(p[I])   # compute cdf
    u[ , n] <- cum_p * l      # rescale to be < 1 at the far right
    Rnk     <- suppressWarnings(pracma::interp1(x = I , y = 1:J , xi = 1:J))
    #Rnk     <- stats::approx(x = I, y = 1:J, xout = 1:J)$x # compute ranking of each entry
    U[ , n] <- cum_p[Rnk] * l # compute grade
  }

  list(ordered_margin = X, ordered_cdf = u, copula = U)

}


# Combination -------------------------------------------------------------

#' @keywords internal
cma_combine <- function(x, u, U) {

  J <- nrow(x)
  K <- ncol(x)
  X <- 0 * U

  for (k in 1:K) {
    X[ , k] <- Hmisc::approxExtrap(
      x      = u[ , k],
      y      = x[ , k],
      xout   = U[ , k],
      method = "linear",
      rule   = 2 ,
      ties   = "ordered")$y
  }

  X

}


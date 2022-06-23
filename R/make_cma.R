
# Separation -------------------------------------------------------------

#' @keywords internal
cma_separate <- function(x, p) {

  J <- nrow(x)
  N <- ncol(x)
  l <- J / (J + 1)
  # p <- apply(cbind(p, 1 / J * 10e-9), 1, max, na.rm = TRUE)
  p[p == 0] <- 10e-9
  p <- p / sum(p)
  u <- 0 * x
  U <- 0 * x

  if (has_colnames(x)) {
    nms <- colnames(x)
  }

  # begin core algorithm
  # MATLAB does this operation in one shot. In R, we have to loop over...
  X    <- 0 * x
  Indx <- 0 * x
  for (n in 1:N) {
    tmp <- sort.int(x[ , n, drop = FALSE], index.return = TRUE)
    X[ , n]    <- tmp$x
    Indx[ , n] <- tmp$ix
  }

  # for each marginal...
  for (n in 1:N) {
    I       <- Indx[ , n]     # sort
    cum_p   <- cumsum(p[I])   # compute cdf
    u[ , n] <- cum_p * l      # rescale to be < 1 at the far right
    Rnk     <- interp_one(x = I , y = 1:J , xi = 1:J)
    #Rnk     <- stats::approx(x = I, y = 1:J, xout = 1:J)$x # compute ranking of each entry
    U[ , n] <- cum_p[Rnk] * l # compute grade
  }

  if (has_colnames(x)) {
    colnames(X) <- nms
    colnames(u) <- nms
    colnames(U) <- nms
  } else {
    colnames(X) <- make_tidy_names(X)
    colnames(u) <- make_tidy_names(u)
    colnames(U) <- make_tidy_names(U)
  }

  list(sorted_margin = X, cdf = u, copula = U)

}


# Combination -------------------------------------------------------------

#' @keywords internal
cma_combine <- function(x, u, U) {

  is_unsorted <- any(apply(x, 2, is.unsorted))
  if (is_unsorted) {
    #warning(
    #  "The marginal distribution was unsorted. Sorting in ascending order.",
    #  immediate. = TRUE
    #)
    x <- apply(x, 2, sort)
  }

  K <- ncol(x)
  X <- 0 * U

  for (k in 1:K) {
    X[ , k] <- approx_extrap(
      x      = u[ , k],
      y      = x[ , k],
      xout   = U[ , k],
      method = "linear",
      rule   = 2 ,
      ties   = "ordered")$y
  }

  tibble::as_tibble(X, .name_repair = "minimal")

}

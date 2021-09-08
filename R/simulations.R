#' Moment Matching Algorithm for the Normal Distribution
#'
#' This function generates scenarios from the normal distribution that match
#' the sample counterparts of given dataset.
#'
#' @param mu A \code{double} with the location parameter.
#' @param sigma A variance-covariance \code{matrix} the dispersion parameter.
#' @param n A \code{integer} with the number of simulations to be conducted.
#'
#' @return #TODO
#' @export
#'
#' @examples
#' x     <- diff(log(EuStockMarkets))
#' mu    <- colMeans(x)
#' sigma <- cov(x)
#' simulate_normal(mu, sigma, 10)
#'
#' cor(x)
#' cor(simulate_normal(mu, sigma, 10000))
#'
#' mu
#' colMeans(simulate_normal(mu, sigma, 10000))
simulate_normal <- function(mu, sigma, n) {

    # # Check Parameters
    # if (method == "CPCA" && is.null(d)) {
    #     method <- "PCA"
    # }
    #
    # if (is.vector(sigma2)) {
    #     sigma2 <- as.matrix(sigma2)
    # }

    # Setup
    n_row <- nrow(sigma)
    half  <- ceiling(n / 2)
    x     <- matrix(rep(0, n_row * n), n, n_row)

    # Step 1: generate standard normal Monte Carlo scenarios
    x_check <- matrix(stats::rnorm(n_row * half), half, n_row)

    # Step 2: store scenarios and set probabilities
    # Store first [j_ / 2] scenarios
    x[1:half, ] <- x_check
    # Generate and store antithetical scenarios
    x_check_anti <- -1 * x_check
    x[(half + 1):n, ] <- x_check_anti[1:(n - half), ]

    # Step 3: twist scenarios
    x_ <- twist_scenarios_mom_match(x = x, mu = mu, sigma = sigma, p = NULL, method = "Riccati")
    x_
}

simulate_t <- function(mu, sigma, nu, n, stoc_rep = FALSE) {

    # if (method == "CPCA" && is.null(d)) {
    #     method <- "PCA"
    # }
    #
    # if (is.vector(sigma)) {
    #     sigma <- as.matrix(sigma)
    # }

    n_row <- nrow(sigma)

    if (stoc_rep == FALSE) {

        # ## Step 1: Riccati root
        sigma <- transpose_square_root(sigma = sigma)

        # ## Step 2: Radial scenarios
        u <- stats::runif(n = n, min = 0, max = 1) # uniform sample
        r <- sqrt(n_row * (stats::qf(u, df1 = n_row, df2 = nu)))

        # ## Step 3: Generate normal scenarios
        n <- simulate_normal(mu = rep(0, n_row), sigma = diag(n_row), n = n)

        # ## Step 4: Uniform component y

        normalizers <- sqrt(rowSums(n^2))
        y <- n / normalizers

        # ## Step 5: Compute Student t scenarios
        x <- as.vector(mu) + r * y %*% sigma

    } else {

        # ## Step 6: Generate normal scenarios
        n <- simulate_normal(mu = rep(0, n_row), sigma = sigma, n = n)

        # ## Step 7: Generate chi-squared scenarios
        v <- stats::rchisq(n = n, df = nu)

        # ## Step 8: Compute Student t scenarios
        x <- t(as.vector(mu) + t(n / (sqrt(v / nu))))
    }

    # Drop unneeded dimensions
    x <- drop(x)
    x

}

#' Moment Matching for the Normal Distribution
#'
#' Generates scenarios from the normal distribution that exactly matches
#' the first two moments.
#'
#' @param mu A \code{double} with the location parameter.
#' @param sigma A variance-covariance \code{matrix} the dispersion parameter.
#' @param n An \code{integer} with the number of scenarios to be generated.
#'
#' @return #TODO
#' @export
#'
#' @seealso \code{\link{match_t}}
#'
#' @examples
#' x     <- diff(log(EuStockMarkets))
#' mu    <- colMeans(x)
#' sigma <- cov(x)
#' match_normal(mu, sigma, 10)
#'
#' # correlation structure is similar
#' cor(x)
#' cor(match_normal(mu, sigma, 10000))
#'
#' # as the location parameters
#' mu
#' colMeans(match_normal(mu, sigma, 10000))
match_normal <- function(mu, sigma, n) {

    n_row <- nrow(sigma)
    half  <- ceiling(n / 2)
    x     <- matrix(rep(0, n_row * n), n, n_row)

    # Monte Carlo scenarios
    x_check <- matrix(stats::rnorm(n_row * half), half, n_row)

    # store scenarios
    x[1:half, ] <- x_check
    # antithetical scenarios
    x_check_anti <- -1 * x_check
    x[(half + 1):n, ] <- x_check_anti[1:(n - half), ]

    # twisting
    twist <- affine_scenarios(x = x, mu = mu, sigma = sigma, p = NULL)

    if (has_colnames(sigma)) {
        colnames(twist) <- colnames(sigma)
    } else if (is.vector(mu)) {
        if (has_names(mu)) {
            colnames(twist) <- names(mu)
        } else if (is.matrix(mu)) {
            if (has_rownames(mu)) {
                colnames(twist) <- rownames(mu)
            }
        }
    } else {
        colnames(twist) <- make_tidy_names(x)
    }

    tibble::as_tibble(twist, .name_repair = "minimal")

}

#' Moment Matching for the t Distribution
#'
#' Generates scenarios from the Student-t distribution that
#' exactly matches the first two moments.
#'
#' @param mu A \code{double} with the location parameter.
#' @param sigma A variance-covariance \code{matrix} the dispersion parameter.
#' @param nu An \code{integer} with the degrees of freedom.
#' @param n An \code{integer} with the number of scenarios to be generated.
#' @param stochastic A \code{logical} flag indicating whether (or not) the stochastic
#' representation should be used. The default if \code{TRUE}.
#'
#' @return A \code{tibble} in the wide format.
#' @export
#'
#' @seealso \code{\link{match_normal}}
#'
#' @examples
#' x     <- diff(log(EuStockMarkets))
#' mu    <- colMeans(x)
#' sigma <- cov(x)
#' nu    <- 5
#' match_t(mu, sigma, nu, 10)
#'
#' cor(x)
#' cor(match_t(mu, sigma, 5, 100000))
#'
#' mu
#' colMeans(match_t(mu, sigma, nu, 100000))
match_t <- function(mu, sigma, nu, n, stochastic = FALSE) {

    n_row <- nrow(sigma)

    if (stochastic) {

        ## Monte-Carlo scenarios
        n <- match_normal(mu = rep(0, n_row), sigma = sigma, n = n)

        # Chi-squared scenarios
        v <- stats::rchisq(n = as.matrix(n), df = nu)

        # Student t scenarios
        x <- t(as.vector(mu) + t(as.matrix(n) / (sqrt(v / nu))))

    } else {

        # riccati root
        sigma <- solve_riccati(sigma)

        # radial scenarios
        u <- stats::runif(n = n, min = 0, max = 1) # uniform sample
        r <- sqrt(n_row * (stats::qf(u, df1 = n_row, df2 = nu)))

        # Monte-Carlo scenarios
        n <- match_normal(mu = rep(0, n_row), sigma = diag(n_row), n = n)

        # uniform component y
        normalizers <- sqrt(rowSums(as.matrix(n) ^ 2))
        y <- as.matrix(n) / normalizers

        # Student t scenarios
        x <- as.vector(mu) + r * y %*% sigma

    }

    if (has_colnames(sigma)) {
        colnames(x) <- colnames(sigma)
    } else if (is.vector(mu)) {
        if (has_names(mu)) {
            colnames(x) <- names(mu)
        } else if (is.matrix(mu)) {
            if (has_rownames(mu)) {
                colnames(x) <- rownames(mu)
            }
        }
    } else {
        colnames(x) <- make_tidy_names(x)
    }

    tibble::as_tibble(x)

}

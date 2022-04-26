#' @keywords internal
affine_scenarios <- function(x, mu, sigma, p = NULL) {

    if (is.vector(x)) {
        x <- as.matrix(x, length(x), 1)
    }

    if (is.vector(sigma)) {
        sigma <- as.matrix(sigma, 1, 1)
    }

    if (is.vector(mu)) {
        mu <- as.matrix(mu, length(mu), 1)
    }

    if (is.null(p)) {
        p <- rep(1 / NROW(x), NROW(x))
    }

    # original moments of x
    result_meancov_sp <- ffp_moments(x, as_ffp(p))

    m_x <- as.matrix(as.vector(result_meancov_sp$mu)) # converts to column vector
    s2_x <- result_meancov_sp$sigma

    # transpose square root of s2_x
    r_x <- solve_riccati(s2_x)

    # transpose square root of s2_
    r <- solve_riccati(sigma)

    # twist matrix
    b <- r %*% solve(r_x)

    # shift vector
    a <- mu - (b %*% m_x)

    # new scenarios
    x <- t(as.vector(a) + b %*% t(x))
    x

}

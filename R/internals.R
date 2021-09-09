#' @keywords internal
solve_riccati <- function(sigma, phi2 = NULL) {

    # Step 1: Hamiltonian matrix (block matrix)
    n_row <- nrow(sigma)

    if (is.null(phi2)) {
        phi2 <- diag(n_row)
    }

    zero_matrix <- matrix(0, n_row, n_row)

    h <- cbind(rbind(zero_matrix, -sigma),
               rbind(-phi2, zero_matrix))

    # Step 2: Schur decomposition
    schur <- QZ::ordqz(h, keyword = "lhp")

    # Step 3: four n_ x n_ partitions
    u <- schur$Q

    u11 <- u[1:n_row, 1:n_row]
    u21 <- u[(n_row + 1):(2 * n_row), 1:n_row]

    # Step 4: Compute Riccati solution
    b <- u21 %*% solve(u11)
    b

}




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
        p <- rep(1 / nrow(x), nrow(x))
    }

    # Step 1: Original moments of x
    result_meancov_sp <- ff_moments(x, p)

    m_x <- as.matrix(as.vector(result_meancov_sp$mu)) # converts to column vector
    s2_x <- result_meancov_sp$sigma

    # Step 2: Transpose square root of s2_x
    r_x <- solve_riccati(s2_x)

    # Step 3: Transpose square root of s2_
    r <- solve_riccati(sigma)

    # Step 4: compute twist matrix
    b <- r %*% solve(r_x)

    # Step 5: compute shift vector
    a <- mu - (b %*% m_x)

    # Step 6: compute new scenarios
    x_ <- t(as.vector(a) + b %*% t(x))
    x_

}

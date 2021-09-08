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
    result_Schur <- QZ::ordqz(h, keyword = "lhp")

    # Step 3: four n_ x n_ partitions
    u <- result_Schur$Q

    u11 <- u[1:n_row, 1:n_row]
    u21 <- u[(n_row + 1):(2 * n_row), 1:n_row]

    # Step 4: Compute Riccati solution
    b <- u21 %*% solve(u11)
    b

}

#' @keywords internal
pca_cov <- function(sigma, k_ = ncol(sigma)) {

    # Error-checking
    if (k_ > ncol(sigma)) {
        k_ <- ncol(sigma)
        warning("k_ value exceeds maximum permissible value, k_ set to max")
    }

    # Find first k_ eigenvalues in decreasing order and the corresponding eigenvectors
    eig_result <- eigen(sigma)

    e <- Re(eig_result$vectors)
    lambda2 <- Re(eig_result$values)

    # enforce a sign convention on the coefficients
    # the largest element in each eigenvector will have a positive sign
    i <- apply(e, 2, function(x) which.max(abs(x)))

    for (k in 1:k_) {
        if (e[i[k], k] < 0) {
            e[, k] <- (-1) * e[, k]
        }
    }

    list(
        lambda2 = lambda2[1:k_], # eigenvalues
        e       = e[,1:k_]       # eigenvectors
    )

}

#' @keywords internal
cpca_cov <- function(sigma, d) {

    n_ <- nrow(sigma)
    k_ <- nrow(d)

    e_d <- matrix(0, n_, n_)
    lambda_d <- rep(0, n_)

    # Step 0: initialize variables
    m_ <- n_ - k_
    a_1_t <- t(d)
    a_1 <- t(a_1_t)

    # Step 1: compute the orthogonal projestion matrix
    for (n in 1:n_) {

        if (n == 1) {

            beta_n <- diag(rep(1, n_)) - t(a_1) %*% solve(a_1 %*% t(a_1)) %*% a_1

            # Step 2: conditional dispersion matrix
            sigma_n <- t(beta_n) %*% sigma %*% beta_n

            # Step 3: conditional principal directions/variances
            result_pca_cov <- pca_cov(sigma_n, 1)
            e_d[ ,1] <- result_pca_cov$e
            lambda_d[1] <- result_pca_cov$lambda2

            # Step 4: update augmented constraints matrix
            a_n1_t <- cbind(t(a_1), sigma %*% e_d[ , 1])

        } else if ((n + 1) < n_) {

            a_n <- t(a_n1_t)
            beta_n <- diag(rep(1, n_)) - t(a_n) %*% solve(a_n %*% t(a_n)) %*% a_n

            # Step 2: conditional dispersion matrix
            sigma_n <- t(beta_n) %*% sigma %*% beta_n

            # Step 3: conditional principal directions/variances
            result_pca_cov <- pca_cov(sigma_n, 1)
            e_d[ ,n] <- result_pca_cov$e
            lambda_d[n] <- result_pca_cov$lambda2

            # Step 4: update augmented constraints matrix
            if (n <= (m_ - 1)) {
                a_n1_t <- cbind(t(a_n), sigma %*% e_d[ , n])

            } else if (m_ <= n && n <= (n_ - 1)) {
                a_n1_t <- sigma %*% e_d[ , 1:n]

            }

        } else {

            a_n <- t(a_n1_t)
            beta_n <- diag(rep(1, n_)) - t(a_n) %*% solve(a_n %*% t(a_n)) %*% a_n

            # Step 2: conditional dispersion matrix
            sigma_n <- t(beta_n) %*% sigma %*% beta_n

            # Step 3: conditional principal directions/variances
            result_pca_cov <- pca_cov(sigma_n, 1)
            e_d[ ,n] <- result_pca_cov$e
            lambda_d[n] <- result_pca_cov$lambda2
        }
    }

    list(lambda2 = lambda_d, e = e_d)

}

#' @keywords internal
gram_schmidt <- function(sigma) {

    n_ <- nrow(sigma)

    # Step 0: initialization
    a <- diag(n_)
    u <- matrix(0, n_, n_ - 1)
    g <- matrix(0, n_, n_)

    # if n = 1
    a_1 <- a[ , 1]
    v_1 <- a_1
    g[ , 1] <- v_1 / as.vector(sqrt(t(v_1) %*% sigma %*% v_1))

    for (n in 2:n_) {

        a_n <- a[ ,n]

        for (m in 1:(n - 1)) {
            # Step 1: projection
            u[ , m] <- (t(g[ , m]) %*% sigma %*% a_n) %*% (g[ , m])
        }

        # Step 2: orthogonalization
        v_n <- a_n - rowSums(u)

        # Step 3: normalization
        g[ ,n] <- v_n / as.vector(sqrt(t(v_n) %*% sigma %*% v_n))

    }

    g

}

#' @keywords internal
transpose_square_root <- function(sigma, method = "Riccati", d = NULL) {

    if (method == "CPCA" && is.null(d)) {
        method <- "PCA"
    }

    # Step 1: Riccati root
    if (method == "Riccati") {
        s <- solve_riccati(sigma = sigma)
    }

    # Step 2: Conditional principal components
    if (method == "CPCA") {
        result_cpca_cov <- cpca_cov(sigma, d)
        lambda <- sqrt(result_cpca_cov$lambda2)
        s <- result_cpca_cov$e %*% diag(lambda)
    }

    # Step 3: Principal components
    if (method == "PCA") {
        result_pca_cov <- pca_cov(sigma)
        lambda <- sqrt(result_pca_cov$lambda2)
        s <- result_pca_cov$e %*% diag(lambda)
    }

    # Step 4: Gram-Schmidt
    if (method == "Gram-Schmidt") {
        result_gram_schmidt <- gram_schmidt(sigma)
        s <- t(solve(result_gram_schmidt))
    }

    # Step 5: Cholesky
    if (method == "Cholesky") {
        s <- chol(sigma)
    }

    s

}

# x,                  # matrix, dim(j_, n_)
# m_,                 # vector, length(n_)
# s2_,                # matrix, dim(n_, n_)
# p = NULL,           # vector, length(j_), optional
# method = "Riccati", # string, optional
# d = NULL            # matrix, dim(n_, k_), optional

#' @keywords internal
twist_scenarios_mom_match <- function(x, mu, sigma, p = NULL, method = "Riccati", d = NULL) {

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

    if (method == "CPCA" && is.null(d)) {
        method <- "PCA"
    }

    # Step 1: Original moments of x
    result_meancov_sp <- ff_moments(x, p)

    m_x <- as.matrix(as.vector(result_meancov_sp$mu)) # converts to column vector
    s2_x <- result_meancov_sp$sigma

    # Step 2: Transpose square root of s2_x
    r_x <- transpose_square_root(s2_x, method, d)

    # Step 3: Transpose square root of s2_
    r <- transpose_square_root(sigma, method, d)

    # Step 4: compute twist matrix
    b <- r %*% solve(r_x)

    # Step 5: compute shift vector
    a <- mu - (b %*% m_x)

    # Step 6: compute new scenarios
    x_ <- t(as.vector(a) + b %*% t(x))
    x_

}

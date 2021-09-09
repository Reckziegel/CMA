panic_copula_test <- function(x, n = 10000, panic_cor = 0.99, panic_prob = 0.02, dist = c("normal", "t")) {

    #assertthat::assert_that(assertthat::is.number(N))
    assertthat::assert_that(assertthat::is.number(n))
    #assertthat::assert_that(assertthat::is.number(data_cor))
    assertthat::assert_that(assertthat::is.number(panic_cor))
    assertthat::assert_that(assertthat::is.number(panic_prob))
    #ssertthat::assert_that(assertthat::is.number(sigma))
    dist <- match.arg(dist, c("normal", "t"))[[1L]]

    N <- ncol(x)
    data_cor <- stats::cor(x)
    #data_cor <- 1 - mean(lower.tri(data_cor))
    sigma <- mean(((1 + apply(x, 2, sd)) ^ 252)) / 100

    # generate panic distribution
    p    <- matrix(1, nrow = n, ncol = 1) / n
    c2_c <- (1 - data_cor) * diag(N) + data_cor * matrix(1, N , N)
    c2_p <- (1 - panic_cor) * diag(N) + panic_cor * matrix(1, N, N)

    s2 <- pracma::blkdiag(c2_c, c2_p)
    if (dist == "normal") {
        Z <- match_normal(mu = matrix(rep(0, N * 2)), sigma = s2, n = n)
    } else {
        Z <- match_t(mu = matrix(rep(0, N * 2)), sigma = s2, nu = 5, n = n)
    }


    X_c <- Z[ , 1:N]
    X_p <- Z[ , (N + 1):ncol(Z)]

    if (dist == "normal") {
        D <- stats::pnorm(X_p) < panic_prob
    } else {
        D <- stats::pt(X_p, df = 5) < panic_prob
    }

    X <- (1 - D) * X_c + (D * X_p)

    # perturb probabilities via Fully Flexible Views
    Aeq <- matrix(1, 1, n) # constrain probabilities to sum to one...
    Aeq <- rbind(Aeq , t(X)) # ...constrain the first moments...
    beq <- 1
    beq <- as.matrix(rbind(beq , matrix(0, N , 1)))
    p_  <- entropy_pooling(p, NULL, NULL, Aeq, beq)

    sep_step <- cma_separation(X, p_)

    # merge panic copula with normal marginals
    y <- NULL
    u <- NULL
    for (n in 1:N) {
        yn <- as.matrix(seq(from = -4 * sigma, to = 4 * sigma, length.out = 1000))
        if (dist == "normal") {
            un <- stats::pnorm(yn , 0, sigma)
        } else {
            un <- stats::pt(yn / sigma, df = 5)
        }

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

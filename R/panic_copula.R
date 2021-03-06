#' Panic-Copula for Stress-Testing
#'
#' This function generates a large dimensional panic copula that can be further
#' used for stress-testing and panic-aware portfolio optimization.
#'
#' @param x A rectangular (non-tidy) data structure.
#' @param n An \code{integer} with the number of scenarios to be generated.
#' @param panic_cor A numeric value for the correlation in panic markets.
#' @param panic_prob A numeric value with the probability in which panic markets
#' can be triggered.
#' @param dist A \code{character} with \code{normal} or \code{t}. The default is
#' \code{normal}
#'
#' @return An object of the \code{panic} class.
#'
#' @export
#'
#' @examples
#' x <- diff(log(EuStockMarkets))
#' panic_copula(x = x, n = 20, panic_cor = 0.99, panic_prob = 0.02)
panic_copula <- function(x, n = 10000, panic_cor = 0.99, panic_prob = 0.02, dist = c("normal", "t")) {

    assertthat::assert_that(assertthat::is.number(n))
    assertthat::assert_that(assertthat::is.number(panic_cor))
    assertthat::assert_that(assertthat::is.number(panic_prob))
    dist <- match.arg(dist, c("normal", "t"))[[1L]]

    N <- ncol(x)
    data_cor <- stats::cor(x)
    #data_cor <- 1 - mean(lower.tri(data_cor))
    sigma <- apply(x, 2, stats::sd)

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
    Z <- as.matrix(Z)

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
    p_  <- entropy_pooling(p, NULL, NULL, Aeq, beq, solver = "nlminb")

    sep_step <- cma_separation(X, p_)

    # merge panic copula with normal marginals
    y <- NULL
    u <- NULL
    if (dist == "t") {
        .df <- fit_t(x)$chi
    }

    for (n in 1:N) {
        yn <- as.matrix(seq(from = -4 * sigma[[n]], to = 4 * sigma[[n]], length.out = 1000))
        if (dist == "normal") {
            un <- stats::pnorm(yn , 0, sigma[[n]])
        } else {
            un <- stats::pt(yn / sigma[[n]], df = .df)
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

    if (has_colnames(x)) {
        colnames(Y) <- colnames(x)
    }

    new_panic_copula(list(simulation = Y, p = p_))

}

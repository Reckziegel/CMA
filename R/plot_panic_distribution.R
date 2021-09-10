#' Visualize a Panic Distribution
#'
#' @param pnl An univariante time series with the PnL marginal distribution.
#' @param p A \code{double} vector of probabilities. If \code{NULL}, `p` is automaticaly
#' set to 1 over n.
#' @param breaks An \code{integer} with the number of break point to be used to
#' plot the marginal panic distribution.
#'
#' @return A \code{ggplot2} object.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' x <- diff(log(EuStockMarkets))
#' x <- panic_copula(x = x, n = 10000, panic_prob = 0.02)
#' w <- rep(0.25, 4)
#' pnl <- x$simulation %*% w
#' plot_panic_distribution(pnl = pnl, p = x$p, 100)
plot_panic_distribution <- function(pnl, p = NULL, breaks) {

    assertthat::assert_that(assertthat::is.number(breaks))
    assert_is_univariate(pnl)
    if (is_empty(p)) {
        p <- rep(1 / NROW(pnl), NROW(pnl))
    } else {
        p <- check_p(p)
    }

    dist <- graphics::hist(x = pnl, breaks = breaks, plot = FALSE)
    n <- dist$counts
    x <- dist$breaks
    d <- x[2] - x[1]

    N  <- length(x)
    np <- matrix(0, nrow = N, ncol = 1)

    for (s in 1:N) {
        index <- (pnl >= x[s] - d / 2) & (pnl <= x[s] + d / 2)
        np[s] <- sum(p[index])
        f     <- np / d
    }
    f <- f / sum(f)

    tibble::tibble(x = as.vector(x), f = as.vector(f)) |>
        ggplot2::ggplot(ggplot2::aes(x = .data$x, y = .data$f)) +
        ggplot2::geom_area() +
        ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 10))

}

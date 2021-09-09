#' Moments under Flexible Probabilities
#'
#' Computes the location and dispersion statistics for scenarios with flexible probabilities.
#'
#' @param x A tabular (non-tidy) time series.
#' @param p A probability vector.
#'
#' @return A \code{list} with 2 elements: \code{mu} and \code{sigma}.
#'
#' @export
#'
#' @examples
#' x <- diff(log(EuStockMarkets))
#' p <- stats::runif(nrow(x))
#' p <- p /  sum(p)
#'
#' ff_moments(x = matrix(x, ncol = 4), p = p)
#'
#' # compare with the stadard approach
#' colMeans(x)
#' cov(x)
ff_moments <- function(x, p = NULL) {
    UseMethod("ff_moments", x)
}

#' @rdname ff_moments
#' @export
ff_moments.default <- function(x, p = NULL) {
    stop("`ff_moments` doesn't know how to deal with the `", class(x)[[1L]], "` class yet.", call. = FALSE)
}

#' @rdname ff_moments
#' @export
ff_moments.numeric <- function(x, p = NULL) {
    ff_moments_(x = x, p = check_p(p))
}

#' @rdname ff_moments
#' @export
ff_moments.matrix <- function(x, p) {
    ff_moments_(x = x, p = check_p(p))
}

#' @rdname ff_moments
#' @export
ff_moments.xts <- function(x, p) {
    ff_moments_(x = as.matrix(x), p = check_p(p))
}

#' @rdname ff_moments
#' @export
ff_moments.data.frame <- function(x, p) {
    numeric_cols <- apply(x, 2, is.numeric)
    assertthat::assert_that(sum(numeric_cols) > 0, msg = "`x` argument must contain at least one numeric column.")
    ff_moments_(x = as.matrix(x[ , numeric_cols]), p = check_p(p))
}

#' @rdname ff_moments
#' @export
ff_moments.tbl <- function(x, p) {
    x <- dplyr::select(x, where(is.numeric))
    assertthat::assert_that(!is_empty(x), msg = "`x` argument must contain at least one numeric column.")
    ff_moments_(x = as.matrix(x), p = check_p(p))
}

#' @keywords internal
ff_moments_ <- function(x, p = NULL) {

    if (is.null(dim(x)) | is.vector(x)) {
        if (is.null(p)) {
            p <- rep(1 / length(x), length(x))
        }

        mu    <- sum(p * x)
        sigma <- sum(p * x * x) - (sum(p * x) * sum(p * x))

    }

    if (!is.null(dim(x)) | is.matrix(x)) {
        if (is.null(p)) {
            p <- rep(1 / nrow(x), nrow(x))
        }

        mu         <- t(x) %*% p
        x_centered <- t(t(x) - as.vector(mu))
        sigma      <- t(x_centered) %*% (x_centered * p %*% (matrix(1, 1, ncol(x))))
    }

    list(mu = mu, sigma = sigma)

}

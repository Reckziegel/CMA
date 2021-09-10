#' Moments under Flexible Probabilities
#'
#' Computes the location and dispersion statistics under flexible probabilities.
#'
#' @param x A tabular (non-tidy) data structure.
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
#' fp_moments(x = matrix(x, ncol = 4), p = p)
#'
#' # compare with the stadard approach
#' colMeans(x)
#' cov(x)
fp_moments <- function(x, p = NULL) {
    UseMethod("fp_moments", x)
}

#' @rdname fp_moments
#' @export
fp_moments.default <- function(x, p = NULL) {
    stop("`fp_moments` doesn't know how to deal with the `", class(x)[[1L]], "` class yet.", call. = FALSE)
}

#' @rdname fp_moments
#' @export
fp_moments.numeric <- function(x, p = NULL) {
    fp_moments_(x = x, p = check_p(p))
}

#' @rdname fp_moments
#' @export
fp_moments.matrix <- function(x, p = NULL) {
    fp_moments_(x = x, p = check_p(p))
}

#' @rdname fp_moments
#' @export
fp_moments.xts <- function(x, p = NULL) {
    fp_moments_(x = as.matrix(x), p = check_p(p))
}

#' @rdname fp_moments
#' @export
fp_moments.data.frame <- function(x, p = NULL) {
    x <- dplyr::select(x, where(is.numeric))
    assertthat::assert_that(!is_empty(x), msg = "`x` argument must contain at least one numeric column.")
    fp_moments_(x = as.matrix(x), p = check_p(p))
}

#' @rdname fp_moments
#' @export
fp_moments.tbl <- function(x, p = NULL) {
    x <- dplyr::select(x, where(is.numeric))
    assertthat::assert_that(!is_empty(x), msg = "`x` argument must contain at least one numeric column.")
    fp_moments_(x = as.matrix(x), p = check_p(p))
}

#' @keywords internal
fp_moments_ <- function(x, p = NULL) {

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



#' Archimedean Copulas for Scenario Analysis
#'
#' Functions to build new types of copulas.
#'
#' @param copula An object of the \code{cma} class.
#' @param method A \code{character} with the method to be used for optimization.
#' The default is \code{mpl}.
#' @param ... Additional arguments to be passed to \code{\link[copula]{archmCopula}}.
#'
#' @return An S3 \code{list} of the \code{cma_copula} class.
#' @export
#'
#' @rdname fit_copula
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#'
#' sep <- cma_separation(x)
#'
#' fit_copula_clayton(sep)
#' fit_copula_gumbel(sep)
fit_copula_clayton <- function(copula, method = c("mpl", "ml", "itau", "irho", "itau.mpl"), ...) {

    if (inherits(copula, "cma_separation")) {

        n_col <- NCOL(copula[[1L]])
        method <- match.arg(method, c("mpl", "ml", "itau", "irho", "itau.mpl"))[[1L]]
        x <- copula::fitCopula(
            copula = copula::claytonCopula(dim = n_col),
            data   = as.matrix(copula$copula),
            method = method,
            ...
        )
        new_copula(x)

    } else {
        stop("`copula` must be an object of the `cma_separation` class.", call. = FALSE)
    }

}

#' @rdname fit_copula
#' @export
fit_copula_gumbel <- function(copula, method = c("mpl", "ml", "itau", "irho", "itau.mpl"), ...) {

    if (inherits(copula, "cma_separation")) {

        n_col <- NCOL(copula[[1L]])
        method <- match.arg(method, c("mpl", "ml", "itau", "irho", "itau.mpl"))[[1L]]
        x <- copula::fitCopula(
            copula = copula::gumbelCopula(dim = n_col),
            data   = as.matrix(copula$copula),
            method = method,
            ...
        )
        new_copula(x)

    } else {
        stop("`copula` must be an object of the `cma_separation` class.", call. = FALSE)
    }

}

#' @rdname fit_copula
#' @export
fit_copula_frank <- function(copula, method = c("mpl", "ml", "itau", "irho", "itau.mpl"), ...) {

    if (inherits(copula, "cma_separation")) {

        n_col <- NCOL(copula[[1L]])
        method <- match.arg(method, c("mpl", "ml", "itau", "irho", "itau.mpl"))[[1L]]
        x <- copula::fitCopula(
            copula = copula::frankCopula(dim = n_col),
            data   = as.matrix(copula$copula),
            method = method,
            ...
        )
        new_copula(x)

    } else {
        stop("`copula` must be an object of the `cma_separation` class.", call. = FALSE)
    }

}

#' @rdname fit_copula
#' @export
fit_copula_t <- function(copula, method = c("mpl", "ml", "itau", "irho", "itau.mpl"), ...) {

    if (inherits(copula, "cma_separation")) {

        n_col <- NCOL(copula[[1L]])
        method <- match.arg(method, c("mpl", "ml", "itau", "irho", "itau.mpl"))[[1L]]
        x <- copula::fitCopula(
            copula = copula::tCopula(dim = n_col),
            data   = as.matrix(copula$copula),
            method = method,
            ...
        )
        new_copula(x)

    } else {
        stop("`copula` must be an object of the `cma_separation` class.", call. = FALSE)
    }

}

#' @rdname fit_copula
#' @export
fit_copula_normal <- function(copula, method = c("mpl", "ml", "itau", "irho", "itau.mpl"), ...) {

    if (inherits(copula, "cma_separation")) {

        n_col <- NCOL(copula[[1L]])
        method <- match.arg(method, c("mpl", "ml", "itau", "irho", "itau.mpl"))[[1L]]
        x <- copula::fitCopula(
            copula = copula::normalCopula(dim = n_col),
            data   = as.matrix(copula$copula),
            method = method,
            ...
        )
        new_copula(x)

    } else {
        stop("`copula` must be an object of the `cma_separation` class.", call. = FALSE)
    }

}

#' @rdname fit_copula
#' @export
fit_copula_joe <- function(copula, method = c("mpl", "ml", "itau", "irho", "itau.mpl"), ...) {

    if (inherits(copula, "cma_separation")) {

        n_col <- NCOL(copula[[1L]])
        method <- match.arg(method, c("mpl", "ml", "itau", "irho", "itau.mpl"))[[1L]]
        x <- copula::fitCopula(
            copula = copula::joeCopula(dim = n_col),
            data   = as.matrix(copula$copula),
            method = method,
            ...
        )
        new_copula(x)

    } else {
        stop("`copula` must be an object of the `cma_separation` class.", call. = FALSE)
    }

}




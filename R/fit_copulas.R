fit_clayton <- function(x, method = c("mpl", "ml", "itau", "irho", "itau.mpl")) {

    if (inherits(x, "cma")) {

        n_col <- NCOL(x[[1L]])
        method <- match.arg(method, c("mpl", "ml", "itau", "irho", "itau.mpl"))[[1L]]
        copula::fitCopula(
            copula = copula::claytonCopula(dim = n_col),
            data   = x$copula,
            method = method
        )

    } else {
        stop("`copula` must be an object of the `cma` class.", call. = FALSE)
    }

}





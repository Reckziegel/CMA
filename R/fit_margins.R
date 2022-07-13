# Generalized Hyperbolic --------------------------------------------------

#' Estimation of the Generalized Hyperbolic Distribution
#'
#' Performs the estimation on the Generalized Hyperbolic distribution
#' (univariate and multivariate). Wrappers \code{\link[ghyp]{fit.ghypuv}} and
#' \code{\link[ghyp]{fit.ghypmv}}.
#'
#' @param x A tabular (non-tidy) data structure.
#' @param symmetric A \code{logical} flag. Should the fitted distribution be symmetric?
#' Defaults to \code{FALSE}.
#'
#' @return A \code{list} of the the class \code{cma_fit} with \code{21} components.
#'
#' @seealso \code{\link{fit_hyp}} \code{\link{fit_nig}} \code{\link{fit_vg}}
#' \code{\link{fit_t}} \code{\link{fit_normal}}
#'
#' @export
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#'
#' # multivariate estimation
#' fit_ghd(x)
#'
#' # univariate estimation
#' fit_ghd(x[ , 3, drop = FALSE])
fit_ghd <- function(x, symmetric = FALSE) {
    UseMethod("fit_ghd", x)
}

#' @rdname fit_ghd
#' @export
fit_ghd.default <- function(x, symmetric = FALSE) {
    rlang::abort("`x` must be a tibble, xts or a matrix.")
}

#' @rdname fit_ghd
#' @export
fit_ghd.tbl <- function(x, symmetric = FALSE) {
    if (any(purrr::map_lgl(x, lubridate::is.Date))) {
        x <- x |>
            timetk::tk_xts(silent = TRUE) |>
            as.matrix()
    } else {
        x <- as.matrix(x)
    }
    fit_ghd_(x = x, symmetric = symmetric)
}

#' @rdname fit_ghd
#' @export
fit_ghd.xts <- function(x, symmetric = FALSE) {
    fit_ghd_(x = as.matrix(x), symmetric = symmetric)
}

#' @rdname fit_ghd
#' @export
fit_ghd.matrix <- function(x, symmetric = FALSE) {
    fit_ghd_(x = x, symmetric = symmetric)
}

#' @keywords internal
fit_ghd_ <- function(x, symmetric = FALSE) {

    assertthat::assert_that(assertthat::is.flag(symmetric))

    if (NCOL(x) == 1) {
        x <- ghyp::fit.ghypuv(data = x, symmetric = symmetric, silent = TRUE)
    } else {
        x <- ghyp::fit.ghypmv(data = x, symmetric = symmetric, silent = TRUE)
    }

    new_cma_fit(x)

}


# Hyperbolic --------------------------------------------------------------

#' Estimation of the Hyperbolic Distribution
#'
#' Performs the estimation on the Hyperbolic distribution
#' (univariate and multivariate). Wrappers \code{\link[ghyp]{fit.hypuv}} and
#' \code{\link[ghyp]{fit.hypmv}}
#'
#' @param x A tabular (non-tidy) data structure.
#' @param symmetric A flag. Should the fitted distribution be symmetric?
#' Defaults to \code{FALSE}.
#'
#' @return A \code{list} of the the class \code{cma_fit} with \code{21} components.
#'
#' @seealso \code{\link{fit_hyp}} \code{\link{fit_nig}} \code{\link{fit_vg}}
#' \code{\link{fit_t}} \code{\link{fit_normal}}
#'
#' @export
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#'
#' # multivariate estimation
#' fit_hyp(x)
#'
#' # univariate estimation
#' fit_hyp(x[ , 4, drop = FALSE])
fit_hyp <- function(x, symmetric = FALSE) {
    UseMethod("fit_hyp", x)
}

#' @rdname fit_hyp
#' @export
fit_hyp.default <- function(x, symmetric = FALSE) {
    rlang::abort("`x` must be a tibble, xts or a matrix.")
}

#' @rdname fit_hyp
#' @export
fit_hyp.tbl <- function(x, symmetric = FALSE) {

    if (any(purrr::map_lgl(x, lubridate::is.Date))) {
        x <- x |>
            timetk::tk_xts(silent = TRUE) |>
            as.matrix()
    } else {
        x <- as.matrix(x)
    }

    fit_hyp_(x = x, symmetric = symmetric)

}

#' @rdname fit_hyp
#' @export
fit_hyp.xts <- function(x, symmetric = FALSE) {
    fit_hyp_(x = as.matrix(x), symmetric = symmetric)
}

#' @rdname fit_hyp
#' @export
fit_hyp.matrix <- function(x, symmetric = FALSE) {
    fit_hyp_(x = x, symmetric = symmetric)
}

#' @keywords internal
fit_hyp_ <- function(x, symmetric = FALSE) {

    assertthat::assert_that(assertthat::is.flag(symmetric))

    if (NCOL(x) == 1) {
        x <- ghyp::fit.hypuv(data = x, symmetric = symmetric, silent = TRUE)
    } else {
        x <- ghyp::fit.hypmv(data = x, symmetric = symmetric, silent = TRUE)
    }

    new_cma_fit(x)

}


# Normal Inverse Gaussian -------------------------------------------------

#' Estimation of the Normal-Inverse Gaussian Distribution
#'
#' Performs the estimation on the Normal-Inverse Gaussian (NIG)
#' distribution (univariate and multivariate). Wrappers \code{\link[ghyp]{fit.NIGuv}}
#' and \code{\link[ghyp]{fit.NIGmv}}.
#'
#' @param x A tabular (non-tidy) data structure.
#' @param symmetric A \code{logical} flag. Should the fitted distribution be symmetric?
#' Defaults to \code{FALSE}.
#'
#' @return A \code{list} of the the class \code{cma_fit} with \code{21} components.
#'
#' @seealso \code{\link{fit_ghd}} \code{\link{fit_hyp}} \code{\link{fit_vg}}
#' \code{\link{fit_t}} \code{\link{fit_normal}}
#'
#' @export
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#'
#' # multivariate estimation
#' fit_nig(x)
#'
#' # univariate estimation
#' fit_nig(x[ , 4, drop = FALSE])
fit_nig <- function(x, symmetric = FALSE) {
    UseMethod("fit_nig", x)
}

#' @rdname fit_nig
#' @export
fit_nig.default <- function(x, symmetric = FALSE) {
    rlang::abort("`x` must be a tibble, xts or a matrix.")
}

#' @rdname fit_nig
#' @export
fit_nig.tbl <- function(x, symmetric = FALSE) {

    if (any(purrr::map_lgl(x, lubridate::is.Date))) {
        x <- x |>
            timetk::tk_xts(silent = TRUE) |>
            as.matrix()
    } else {
        x <- as.matrix(x)
    }

    fit_nig_(x = x, symmetric = symmetric)

}

#' @rdname fit_nig
#' @export
fit_nig.xts <- function(x, symmetric = FALSE) {
    fit_nig_(x = as.matrix(x), symmetric = symmetric)
}

#' @rdname fit_nig
#' @export
fit_nig.matrix <- function(x, symmetric = FALSE) {
    fit_nig_(x = x, symmetric = symmetric)
}

#' @keywords internal
fit_nig_ <- function(x, symmetric = FALSE) {

    assertthat::assert_that(assertthat::is.flag(symmetric))

    if (NCOL(x) == 1) {
        x <- ghyp::fit.NIGuv(data = x, symmetric = symmetric, silent = TRUE)
    } else {
        x <- ghyp::fit.NIGmv(data = x, symmetric = symmetric, silent = TRUE)
    }

    new_cma_fit(x)

}


# Variance-Gamma ----------------------------------------------------------

#' Estimation of the Variance-Gamma Distribution
#'
#' Performs the estimation on the Variance-Gamma (VG)
#' distribution (univariate and multivariate). Wrappers \code{\link[ghyp]{fit.VGuv}}
#' and \code{\link[ghyp]{fit.VGmv}}.
#'
#' @param x A tabular (non-tidy) data structure.
#' @param symmetric A \code{logical} flag. Should the fitted distribution be symmetric?
#' Defaults to \code{FALSE}.
#'
#' @return A \code{list} of the the class \code{cma_fit} with \code{21} components.
#'
#' @seealso \code{\link{fit_ghd}} \code{\link{fit_hyp}} \code{\link{fit_nig}}
#' \code{\link{fit_t}} \code{\link{fit_normal}}
#'
#' @export
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#'
#' # multivariate estimation
#' fit_vg(x[ , 3:4])
#'
#' # univariate estimation
#' fit_vg(x[ , 4, drop = FALSE])
fit_vg <- function(x, symmetric = FALSE) {
    UseMethod("fit_vg", x)
}

#' @rdname fit_vg
#' @export
fit_vg.default <- function(x, symmetric = FALSE) {
    rlang::abort("`x` must be a tibble, xts or a matrix.")
}

#' @rdname fit_vg
#' @export
fit_vg.tbl <- function(x, symmetric = FALSE) {

    if (any(purrr::map_lgl(x, lubridate::is.Date))) {
        x <- x |>
            timetk::tk_xts(silent = TRUE) |>
            as.matrix()
    } else {
        x <- as.matrix(x)
    }

    fit_vg_(x = x, symmetric = symmetric)

}

#' @rdname fit_vg
#' @export
fit_vg.xts <- function(x, symmetric = FALSE) {
    fit_vg_(x = as.matrix(x), symmetric = symmetric)
}

#' @rdname fit_vg
#' @export
fit_vg.matrix <- function(x, symmetric = FALSE) {
    fit_vg_(x = x, symmetric = symmetric)
}

#' @keywords internal
fit_vg_ <- function(x, symmetric = FALSE) {

    assertthat::assert_that(assertthat::is.flag(symmetric))

    if (NCOL(x) == 1) {
        x <- ghyp::fit.VGuv(data = x, symmetric = symmetric, silent = TRUE)
    } else {
        x <- ghyp::fit.VGmv(data = x, symmetric = symmetric, silent = TRUE)
    }

    new_cma_fit(x)

}


# Student-t ---------------------------------------------------------------

#' Estimation of the Student-t Distribution
#'
#' Performs the estimation on the Student-t distribution
#' (univariate and multivariate). Wrappers \code{\link[ghyp]{fit.tuv}}
#' and \code{\link[ghyp]{fit.tmv}}.
#'
#' @param x A tabular (non-tidy) data structure.
#' @param symmetric A \code{logical} flag. Should the fitted distribution be symmetric?
#' Defaults to \code{FALSE}.
#'
#' @return A \code{list} of the the class \code{cma_fit} with \code{21} components.
#'
#' @seealso \code{\link{fit_ghd}} \code{\link{fit_hyp}} \code{\link{fit_nig}}
#' \code{\link{fit_vg}} \code{\link{fit_normal}}
#'
#' @export
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#'
#' # multivariate estimation
#' fit_t(x)
#'
#' # univariate estimation
#' fit_t(x[ , 4, drop = FALSE])
fit_t <- function(x, symmetric = FALSE) {
    UseMethod("fit_t", x)
}

#' @rdname fit_t
#' @export
fit_t.default <- function(x, symmetric = FALSE) {
    rlang::abort("`x` must be a tibble, xts or a matrix.")
}

#' @rdname fit_t
#' @export
fit_t.tbl <- function(x, symmetric = FALSE) {

    if (any(purrr::map_lgl(x, lubridate::is.Date))) {
        x <- x |>
            timetk::tk_xts(silent = TRUE) |>
            as.matrix()
    } else {
        x <- as.matrix(x)
    }

    fit_t_(x = x, symmetric = symmetric)

}

#' @rdname fit_t
#' @export
fit_t.xts <- function(x, symmetric = FALSE) {
    fit_t_(x = as.matrix(x), symmetric = symmetric)
}

#' @rdname fit_t
#' @export
fit_t.matrix <- function(x, symmetric = FALSE) {
    fit_t_(x = x, symmetric = symmetric)
}

#' @keywords internal
fit_t_ <- function(x, symmetric = FALSE) {

    assertthat::assert_that(assertthat::is.flag(symmetric))

    if (NCOL(x) == 1) {
        x <- ghyp::fit.tuv(data = x, symmetric = symmetric, silent = TRUE)
    } else {
        x <- ghyp::fit.tmv(data = x, symmetric = symmetric, silent = TRUE)
    }

    new_cma_fit(x)

}


# Normal Distribution -----------------------------------------------------

#' Estimation of the Normal Distribution
#'
#' Performs the estimation on the normal distribution
#' (univariate and multivariate). Wrappers \code{\link[ghyp]{fit.gaussuv}}
#' and \code{\link[ghyp]{fit.gaussmv}}.
#'
#' @param x A tabular (non-tidy) data structure.
#'
#' @return A \code{list} of the the class \code{cma_fit} with \code{21} components.
#'
#' @seealso \code{\link{fit_ghd}} \code{\link{fit_hyp}} \code{\link{fit_nig}}
#' \code{\link{fit_vg}} \code{\link{fit_t}}
#'
#' @export
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#'
#' # multivariate estimation
#' fit_normal(x)
#'
#' # univariate estimation
#' fit_normal(x[ , 4, drop = FALSE])
fit_normal <- function(x) {
    UseMethod("fit_normal", x)
}

#' @rdname fit_normal
#' @export
fit_normal.default <- function(x) {
    rlang::abort("`x` must be a tibble, xts or a matrix.")
}

#' @rdname fit_normal
#' @export
fit_normal.tbl <- function(x) {

    if (any(purrr::map_lgl(x, lubridate::is.Date))) {
        x <- x |>
            timetk::tk_xts(silent = TRUE) |>
            as.matrix()
    } else {
        x <- as.matrix(x)
    }

    fit_normal_(x = x)

}

#' @rdname fit_normal
#' @export
fit_normal.xts <- function(x) {
    fit_normal_(x = as.matrix(x))
}

#' @rdname fit_normal
#' @export
fit_normal.matrix <- function(x) {
    fit_normal_(x = x)
}

#' @keywords internal
fit_normal_ <- function(x) {

    if (NCOL(x) == 1) {
        x <- ghyp::fit.gaussuv(data = x)
    } else {
        x <- ghyp::fit.gaussmv(data = x)
    }

    new_cma_fit(x)

}

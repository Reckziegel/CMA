# Generalized Hyperbolic --------------------------------------------------

#' Estimation of the Generalized Hyperbolic Distribution
#'
#' This function is a wrapper around the \code{\link[ghyp]{fit.ghypuv}} and
#' \code{\link[ghyp]{fit.ghypmv}}, that performs a maximum likelihood estimation
#' on generalized hyperbolic distribution.
#'
#' @param .invariant A tabular data structure.
#' @param .symmetric A flag. Should the estimated distribution be symmetric?
#' Defaults to \code{FALSE}.
#'
#' @return An object of the the class \code{mle.ghyp}.
#'
#' @seealso \code{\link{fit_hyp}} \code{\link{fit_nig}} \code{\link{fit_vg}} \code{\link{fit_t}}
#'
#' @export
#'
#' @examples
#' ##### fit_ghd #####
#'
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#'
#' fit_ghd(x) # multivariate estimation
#' fit_ghd(x[ , 3, drop = FALSE]) # univariate estimation
fit_ghd <- function(.invariant, .symmetric = FALSE) {
    UseMethod("fit_ghd", .invariant)
}

#' @rdname fit_ghd
#' @export
fit_ghd.default <- function(.invariant, .symmetric = FALSE) {
    stop("`.invariant` must be a tibble, xts or a matrix.", call. = FALSE)
}

#' @rdname fit_ghd
#' @export
fit_ghd.tbl <- function(.invariant, .symmetric = FALSE) {
    if (any(purrr::map_lgl(.invariant, lubridate::is.Date))) {
        .invariant <- .invariant |>
            timetk::tk_xts(silent = TRUE) |>
            as.matrix()
    } else {
        .invariant <- as.matrix(.invariant)
    }
    fit_ghd_(.invariant = .invariant, .symmetric = .symmetric)
}

#' @rdname fit_ghd
#' @export
fit_ghd.xts <- function(.invariant, .symmetric = FALSE) {
    fit_ghd_(.invariant = as.matrix(.invariant), .symmetric = .symmetric)
}

#' @rdname fit_ghd
#' @export
fit_ghd.matrix <- function(.invariant, .symmetric = FALSE) {
    fit_ghd_(.invariant = .invariant, .symmetric = .symmetric)
}

#' @keywords internal
fit_ghd_ <- function(.invariant, .symmetric = FALSE) {

    assertthat::assert_that(assertthat::is.flag(.symmetric))

    #stop("fit_generalized_hyperbolic works only in univariate data. Adjust your call.", call. = FALSE)
    if (NCOL(.invariant) == 1) {
        x <- ghyp::fit.ghypuv(data = .invariant, symmetric = .symmetric, silent = TRUE)
    } else {
        x <- ghyp::fit.ghypmv(data = .invariant, symmetric = .symmetric, silent = TRUE)
    }
    new_cma_fit(x)

}


# Hyperbolic --------------------------------------------------------------


#' Estimation of the Hyperbolic Distribution
#'
#' This function is a wrapper around the \code{\link[ghyp]{fit.hypuv}} and
#' \code{\link[ghyp]{fit.hypmv}}, that performs a maximum likelihood estimation
#' on the hyperbolic distribution.
#'
#' @param .invariant A tabular data structure.
#' @param .symmetric A flag. Should the estimated distribution be symmetric?
#' Defaults to \code{FALSE}.
#'
#' @return An object of the the class \code{mle.ghyp}.
#'
#' @seealso \code{\link{fit_hyp}} \code{\link{fit_nig}} \code{\link{fit_vg}} \code{\link{fit_t}}
#'
#' @export
#'
#' @examples
#' ##### fit_hyp #####
#'
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#'
#' fit_hyp(x)     # multivariate estimation
#' fit_hyp(x[ , 4, drop = FALSE]) # univariate estimation
fit_hyp <- function(.invariant, .symmetric = FALSE) {
    UseMethod("fit_hyp", .invariant)
}

#' @rdname fit_hyp
#' @export
fit_hyp.default <- function(.invariant, .symmetric = FALSE) {
    stop("`.invariant` must be a tibble, xts or a matrix.", call. = FALSE)
}

#' @rdname fit_hyp
#' @export
fit_hyp.tbl <- function(.invariant, .symmetric = FALSE) {
    if (any(purrr::map_lgl(.invariant, lubridate::is.Date))) {
        .invariant <- .invariant |>
            timetk::tk_xts(silent = TRUE) |>
            as.matrix()
    } else {
        .invariant <- as.matrix(.invariant)
    }
    fit_hyp_(.invariant = .invariant, .symmetric = .symmetric)

}

#' @rdname fit_hyp
#' @export
fit_hyp.xts <- function(.invariant, .symmetric = FALSE) {
    fit_hyp_(.invariant = as.matrix(.invariant), .symmetric = .symmetric)
}

#' @rdname fit_hyp
#' @export
fit_hyp.matrix <- function(.invariant, .symmetric = FALSE) {
    fit_hyp_(.invariant = .invariant, .symmetric = .symmetric)
}

#' @keywords internal
fit_hyp_ <- function(.invariant, .symmetric = FALSE) {

    assertthat::assert_that(assertthat::is.flag(.symmetric))

    if (NCOL(.invariant) == 1) {
        x <- ghyp::fit.hypuv(data = .invariant, symmetric = .symmetric, silent = TRUE)
    } else {
        x <- ghyp::fit.hypmv(data = .invariant, symmetric = .symmetric, silent = TRUE)
    }
    new_cma_fit(x)

}


# Normal Inverse Gaussian -------------------------------------------------

#' Estimation of the Normal-Inverse Gaussian Distribution
#'
#' This function is a wrapper around the \code{\link[ghyp]{fit.NIGuv}} and
#' \code{\link[ghyp]{fit.NIGmv}}, that performs a maximum likelihood estimation
#' on the Normal-Inverse Gaussian (NIG) distribution.
#'
#' @param .invariant A tabular data structure.
#' @param .symmetric A flag. Should the estimated distribution be symmetric?
#' Defaults to \code{FALSE}.
#'
#' @return An object of the the class \code{mle.ghyp}.
#'
#' @seealso \code{\link{fit_ghd}} \code{\link{fit_hyp}} \code{\link{fit_vg}} \code{\link{fit_t}}
#'
#' @export
#'
#' @examples
#' ##### fit_nig #####
#'
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#'
#' fit_nig(x)     # multivariate estimation
#' fit_nig(x[ , 4, drop = FALSE]) # univariate estimation
fit_nig <- function(.invariant, .symmetric = FALSE) {
    UseMethod("fit_nig", .invariant)
}

#' @rdname fit_nig
#' @export
fit_nig.default <- function(.invariant, .symmetric = FALSE) {
    stop("`.invariant` must be a tibble, xts or a matrix.", call. = FALSE)
}

#' @rdname fit_nig
#' @export
fit_nig.tbl <- function(.invariant, .symmetric = FALSE) {
    if (any(purrr::map_lgl(.invariant, lubridate::is.Date))) {
        .invariant <- .invariant |>
            timetk::tk_xts(silent = TRUE) |>
            as.matrix()
    } else {
        .invariant <- as.matrix(.invariant)
    }
    fit_nig_(.invariant = .invariant, .symmetric = .symmetric)
}

#' @rdname fit_nig
#' @export
fit_nig.xts <- function(.invariant, .symmetric = FALSE) {
    fit_nig_(.invariant = as.matrix(.invariant), .symmetric = .symmetric)
}

#' @rdname fit_nig
#' @export
fit_nig.matrix <- function(.invariant, .symmetric = FALSE) {
    fit_nig_(.invariant = .invariant, .symmetric = .symmetric)
}

#' @keywords internal
fit_nig_ <- function(.invariant, .symmetric = FALSE) {

    assertthat::assert_that(assertthat::is.flag(.symmetric))

    if (NCOL(.invariant) == 1) {
        x <- ghyp::fit.NIGuv(data = .invariant, symmetric = .symmetric, silent = TRUE)
    } else {
        x <- ghyp::fit.NIGmv(data = .invariant, symmetric = .symmetric, silent = TRUE)
    }
    new_cma_fit(x)

}




# Variance-Gamma ----------------------------------------------------------

#' Estimation of the Variance-Gamma Distribution
#'
#' This function is a wrapper around the \code{\link[ghyp]{fit.VGuv}} and
#' \code{\link[ghyp]{fit.VGmv}}, that performs a maximum likelihood estimation
#' on the Variance-Gamma (VG) distribution.
#'
#' @param .invariant A tabular data structure.
#' @param .symmetric A flag. Should the estimated distribution be symmetric?
#' Defaults to \code{FALSE}.
#'
#' @return An object of the the class \code{mle.ghyp}.
#'
#' @seealso \code{\link{fit_ghd}} \code{\link{fit_hyp}} \code{\link{fit_nig}} \code{\link{fit_t}}
#'
#' @export
#'
#' @examples
#' ##### fit_vg #####
#'
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#'
#' fit_vg(x)     # multivariate estimation
#' fit_vg(x[ , 4, drop = FALSE]) # univariate estimation
fit_vg <- function(.invariant, .symmetric = FALSE) {
    UseMethod("fit_vg", .invariant)
}

#' @rdname fit_vg
#' @export
fit_vg.default <- function(.invariant, .symmetric = FALSE) {
    stop("`.invariant` must be a tibble, xts or a matrix.", call. = FALSE)
}

#' @rdname fit_vg
#' @export
fit_vg.tbl <- function(.invariant, .symmetric = FALSE) {
    if (any(purrr::map_lgl(.invariant, lubridate::is.Date))) {
        .invariant <- .invariant |>
            timetk::tk_xts(silent = TRUE) |>
            as.matrix()
    } else {
        .invariant <- as.matrix(.invariant)
    }
    fit_vg_(.invariant = .invariant, .symmetric = .symmetric)
}

#' @rdname fit_vg
#' @export
fit_vg.xts <- function(.invariant, .symmetric = FALSE) {
    fit_vg_(.invariant = as.matrix(.invariant), .symmetric = .symmetric)
}

#' @rdname fit_vg
#' @export
fit_vg.matrix <- function(.invariant, .symmetric = FALSE) {
    fit_vg_(.invariant = .invariant, .symmetric = .symmetric)
}

#' @keywords internal
fit_vg_ <- function(.invariant, .symmetric = FALSE) {

    assertthat::assert_that(assertthat::is.flag(.symmetric))

    if (NCOL(.invariant) == 1) {
        x <- ghyp::fit.VGuv(data = .invariant, symmetric = .symmetric, silent = TRUE)
    } else {
        x <- ghyp::fit.VGmv(data = .invariant, symmetric = .symmetric, silent = TRUE)
    }
    new_cma_fit(x)

}


# Student-t ---------------------------------------------------------------

#' Estimation of the Student-t Distribution
#'
#' This function is a wrapper around the \code{\link[ghyp]{fit.tuv}} and
#' \code{\link[ghyp]{fit.tmv}}, that performs a maximum likelihood estimation
#' on the Student-t distribution.
#'
#' @param .invariant A tabular data structure.
#' @param .symmetric A flag. Should the estimated distribution be symmetric?
#' Defaults to \code{FALSE}.
#'
#' @return An object of the the class \code{mle.ghyp}.
#'
#' @seealso \code{\link{fit_ghd}} \code{\link{fit_hyp}} \code{\link{fit_nig}} \code{\link{fit_vg}}
#'
#' @export
#'
#' @examples
#' ##### fit_t #####
#'
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#'
#' fit_t(x)     # multivariate estimation
#' fit_t(x[ , 4, drop = FALSE]) # univariate estimation
fit_t <- function(.invariant, .symmetric = FALSE) {
    UseMethod("fit_t", .invariant)
}

#' @rdname fit_t
#' @export
fit_t.default <- function(.invariant, .symmetric = FALSE) {
    stop("`.invariant` must be a tibble, xts or a matrix.", call. = FALSE)
}

#' @rdname fit_t
#' @export
fit_t.tbl <- function(.invariant, .symmetric = FALSE) {
    if (any(purrr::map_lgl(.invariant, lubridate::is.Date))) {
        .invariant <- .invariant |>
            timetk::tk_xts(silent = TRUE) |>
            as.matrix()
    } else {
        .invariant <- as.matrix(.invariant)
    }
    fit_t_(.invariant = .invariant, .symmetric = .symmetric)
}

#' @rdname fit_t
#' @export
fit_t.xts <- function(.invariant, .symmetric = FALSE) {
    fit_t_(.invariant = as.matrix(.invariant), .symmetric = .symmetric)
}

#' @rdname fit_t
#' @export
fit_t.matrix <- function(.invariant, .symmetric = FALSE) {
    fit_t_(.invariant = .invariant, .symmetric = .symmetric)
}

#' @keywords internal
fit_t_ <- function(.invariant, .symmetric = FALSE) {

    assertthat::assert_that(assertthat::is.flag(.symmetric))

    if (NCOL(.invariant) == 1) {
        x <- ghyp::fit.tuv(data = .invariant, symmetric = .symmetric, silent = TRUE)
    } else {
        x <- ghyp::fit.tmv(data = .invariant, symmetric = .symmetric, silent = TRUE)
    }
    new_cma_fit(x)

}




# Printing Methods --------------------------------------------------------

#' Internal function to create a prayer fit class.
#'
#' @param x A fitted object
#' @param ... Any arguments to be passed as attributes
#'
#' @return A \code{list} of the \code{prayer_fit} class.
#' @keywords internal
new_cma_fit <- function(x, ...) {

    if (inherits(x, "mle.ghyp")) {

        out <- list()
        out$n_iter             <- x@n.iter
        out$llh                <- x@llh
        out$converged          <- x@converged
        out$error_code         <- x@error.code
        out$error_message      <- x@error.message
        out$aic                <- x@aic
        out$parameter_variance <- x@parameter.variance
        out$trace_pars         <- x@trace.pars
        out$call               <- x@call
        out$lambda             <- x@lambda
        out$alpha_bar          <- x@alpha.bar
        out$chi                <- x@chi
        out$psi                <- x@psi
        out$mu                 <- x@mu
        out$sigma              <- x@sigma
        out$model              <- x@model
        out$dimension          <- x@dimension
        out$expected_value     <- x@expected.value
        out$variance           <- x@variance
        out$parametrization    <- x@parametrization
        out$data               <- x@data

        vctrs::new_list_of(x = out, ptype = double(), ghyp = x, ..., class = "cma_fit")

    }
}

#' @rdname new_cma_fit
#' @importFrom vctrs obj_print_data
#' @export
obj_print_data.cma_fit <- function(x, ...) {
    if (x$converged) {
        cat("Converged:      ", x$converged)
        cat("\n")
        cat("Dimension:      ", x$dimension)
        cat("\n")
        cat("AIC:           ", x$aic)
        cat("\n")
        cat("Log-Likelihood: ", x$llh)
        cat("\n")
        cat("Model:          " , x$model[[1L]])
    } else {
        cat("Converged:    ", x$converged)
        cat("\n")
        cat("Error Code:   ", x$error_code)
        cat("\n")
        cat("Error Message:", x$error_message)
        cat("\n")
    }
}

# for compatibility with the S4 system
methods::setOldClass(c("cma_fit", "vctrs_vctr"))

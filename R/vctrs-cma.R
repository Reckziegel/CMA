
# CMA ---------------------------------------------------------------------

#' Object created to make printing prettier.
#'
#' @param x An object
#' @param ... Additional arguments.
#'
#' @return An object of the cma class.
#'
#' @keywords internal
new_cma_separation <- function(x, ...) {

  dots <- as.list(...)
  if (!has_names(x)) {
    colnames(x) <- make_tidy_names(x)
  }
  vctrs::new_list_of(
    x          = list(
      marginal = tibble::as_tibble(x, .name_repair = "minimal"),
      cdf      = tibble::as_tibble(dots$cdf, .name_repair = "minimal"),
      copula   = tibble::as_tibble(dots$copula, .name_repair = "minimal")
      ),
    ptype      = double(),
    ord_margin = dots$sorted_margin,
    class      = "cma_separation"
  )

}


#' @importFrom vctrs obj_print_header
#' @export
obj_print_header.cma_separation <- function(x, ...) {
  cat(crayon::cyan("# CMA Decomposition"))
}

#' @importFrom vctrs obj_print_data
#' @export
obj_print_data.cma_separation <- function(x, ...) {
  cat("\n")
  cat("marginal: <<", crayon::silver("tbl"), NROW(x$marginal), "x", NCOL(x$marginal),">>")
  cat("\n")
  cat("cdf     : <<", crayon::silver("tbl"), NROW(x$cdf), "x", NCOL(x$cdf),">>")
  cat("\n")
  cat("copula  : <<", crayon::silver("tbl"), NROW(x$copula), "x", NCOL(x$copula),">>")
  cat("\n")
}


# Marginal ----------------------------------------------------------------


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
    out$loglik             <- x@llh
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

#' @importFrom vctrs obj_print_header
#' @export
obj_print_header.cma_fit <- function(x, ...) {
  cat(crayon::silver("# Margins Estimation"))
  cat("\n")
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
    cat("Log-Likelihood: ", x$loglik)
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



# New marginals -----------------------------------------------------------


#' Internal vctrs methods
#'
#' @param x A numeric vector.
#' @return No return value, called for side effects.
#' @import vctrs
#' @keywords internal
#' @name cma-marginal
NULL

# for compatibility with the S4 system
methods::setOldClass(c("marginal", "vctrs_vctr"))

#' @rdname cma-marginal
new_marginal <- function(x, ...) {
  dots <- as.list(...)
  if (!has_names(x)) {
    colnames(x) <- make_tidy_names(x)
  }
  vctrs::new_list_of(x     = list(marginal = tibble::as_tibble(x)),
                     ptype = double(),
                     model = dots$model,
                     class = "marginal"
  )
}

#' @importFrom vctrs obj_print_header
#' @export
obj_print_header.marginal <- function(x, ...) {
  cat(crayon::green("# New Margins"))
}

#' @rdname cma-marginal
#' @export
obj_print_data.marginal <- function(x, ...) {
    cat(crayon::cyan(attributes(x)$model))
    cat("\n")
    cat("marginal: <<", crayon::silver("tbl"), NROW(x$marginal), "x", NCOL(x$marginal),">>")
}


# Copulas -----------------------------------------------------------------

#' Internal vctrs methods
#'
#' @param x A numeric vector.
#' @return No return value, called for side effects.
#' @import vctrs
#' @keywords internal
#' @name cma-copula
NULL

# for compatibility with the S4 system
methods::setOldClass(c("cma_copula", "vctrs_vctr"))

#' @rdname cma-copula
new_copula <- function(x, ...) {

  if (inherits(x, "fitCopula")) {

    out <- list()
    out$estimate  <- x@estimate
    out$var.est   <- c(x@var.est)
    out$loglik    <- x@loglik
    out$dimension <- x@copula@dimension
    out$nsample   <- x@nsample
    out$method    <- x@method
    out$call      <- x@call
    out$converged <- x@fitting.stats$convergence
    out$counts    <- x@fitting.stats$counts

    vctrs::new_list_of(x = out, ptype = double(), class = "cma_copula", copula = x, model = class(x@copula)[[1L]])

  } else {

    stop("`new_copula` can only be used with objects from the `fitCopula` class.", call. = FALSE)

  }

}

#' @importFrom vctrs obj_print_header
#' @export
obj_print_header.cma_copula <- function(x, ...) {
  cat(crayon::blue("# New Copula"))
  cat("\n")
}

#' @rdname cma-copula
#' @export
obj_print_data.cma_copula <- function(x, ...) {
  param <- attributes(x)$copula@copula@param.names
  dim   <- attributes(x)$copula@copula@dimension
  cat("Conveged:      ", x$converged)
  cat("\n")
  cat("Dimension:     ", dim)
  cat("\n")
  cat("Log-Likelihood:", x$loglik)
  cat("\n")
  cat("Model:         ", attributes(x)$model)
}


# Panic Copula ------------------------------------------------------------

#' Internal vctrs methods
#'
#' @param x A numeric vector.
#' @return No return value, called for side effects.
#' @import vctrs
#' @keywords internal
#' @name cma-panic
NULL

#' @rdname cma-panic
new_panic_copula <- function(x, ...) {
  vctrs::new_list_of(x = x, ptype = double(), class = "panic_copula")
}

# for compatibility with the S4 system
methods::setOldClass(c("panic_copula", "vctrs_vctr"))

#' @importFrom vctrs obj_print_header
#' @export
obj_print_header.panic_copula <- function(x, ...) {
  cat(crayon::blue("# Panic Copula"))
  cat("\n")
}

#' @rdname cma-copula
#' @export
obj_print_data.panic_copula <- function(x, ...) {
  cat("simulation: <<", crayon::silver("tbl"), NROW(x$simulation), "x", NCOL(x$simulation),">>")
  cat("\n")
  cat("p:          <<", crayon::silver("tbl"), NROW(x$p), "x", NCOL(x$p),">>")
  cat("\n")
}

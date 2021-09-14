
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
    x          = list(marginal = x, cdf = dots$cdf, copula = dots$copula),
    ptype      = double(),
    ord_margin = dots$sorted_margin,
    class      = "cma",
    kind       = "separation"
  )

}


#' @importFrom vctrs obj_print_header
#' @export
obj_print_header.cma <- function(x, ...) {
  cat(crayon::cyan("# CMA Decomposition"))
}

#' @importFrom vctrs obj_print_data
#' @export
obj_print_data.cma <- function(x, ...) {
  cat("\n")
  cat("marginal: << dim", NROW(x$marginal), "x", NCOL(x$marginal),">>")
  cat("\n")
  cat("cdf     : << dim", NROW(x$cdf), "x", NCOL(x$cdf),">>")
  cat("\n")
  cat("copula  : << dim", NROW(x$copula), "x", NCOL(x$copula),">>")
  cat("\n")
  #x <- new_cma_to_print(tibble::enframe(x, name = "argument"))
  #print(x)
}


# Marginal ----------------------------------------------------------------


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
#' @export
new_marginal <- function(x, ...) {
  dots <- as.list(...)
  if (!has_names(x)) {
    colnames(x) <- make_tidy_names(x)
  }
  vctrs::new_list_of(x     = list(marginal = x),
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
    cat("marginal: << dim", NROW(x$marginal), "x", NCOL(x$marginal),">>")
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
#' @export
new_copula <- function(x, ...) {
  dots <- as.list(...)
  if (!has_names(x)) {
    colnames(x) <- make_tidy_names(x)
  }
  vctrs::new_list_of(x     = list(copula = x),
                     ptype = double(),
                     class = "cma_copula"
  )
}

#' @importFrom vctrs obj_print_header
#' @export
obj_print_header.cma_copula <- function(x, ...) {
  cat(crayon::blue("# New Copula"))
}

#' @rdname cma-copula
#' @export
obj_print_data.cma_copula <- function(x, ...) {
  cat("\n")
  cat("this is a test")
  #cat(crayon::cyan(attributes(x)$model))
  #cat("\n")
  #cat("marginal: << dim", NROW(x$marginal), "x", NCOL(x$marginal),">>")
}

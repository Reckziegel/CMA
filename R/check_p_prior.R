#' Functions to check the consistency of prior probabilities.
#'
#' @param p A vector, a matrix, a xts or a tibble object.
#'
#' @return A matrix object with 1 column.
#'
#' @keywords internal
#'
#' @examples
#' #
check_p_prior <- function(p) {
  UseMethod("check_p_prior", p)
}

#' @rdname check_p_prior
check_p_prior.numeric <- function(p) {
  p <- as.matrix(p)
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("The probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.00001)) {
    stop("The probabilities cann't be negative.", .call = FALSE)
  }
  p
}

#' @rdname check_p_prior
check_p_prior.matrix <- function(p) {
  p <- matrix(p, ncol = 1)
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("The probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.0001)) {
    stop("The probabilities cann't be negative.", .call = FALSE)
  }
  p
}

#' @rdname check_p_prior
check_p_prior.xts <- function(p) {
  p <- matrix(p, ncol = 1)
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("The probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.0001)) {
    stop("The probabilities cann't be negative.", .call = FALSE)
  }
  p
}

#' @rdname check_p_prior
check_p_prior.tbl <- function(p) {
  p <- matrix(dplyr::select_if(p, is.numeric), ncol = 1)
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("The probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.0001)) {
    stop("The probabilities cann't be negative.", .call = FALSE)
  }
  p
}


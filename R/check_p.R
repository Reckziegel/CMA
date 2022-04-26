#' Internal functions to check the consistency of probabilities.
#'
#' @param p A vector, a matrix, a xts or a tibble object.
#'
#' @return A matrix object with 1 column.
#'
#' @examples
#' #
check_p <- function(p) {
  UseMethod("check_p", p)
}

#' @rdname check_p
#' @export
check_p.default <- function(p) {
  stop("CMA doesn't support the `", class(p)[[1L]], "` yet.", call. = FALSE)
}

#' @rdname check_p
#' @export
check_p.NULL <- function(p) NULL

#' @rdname check_p
#' @export
check_p.numeric <- function(p) {
  p <- as.matrix(as.double(p))
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("Probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.00001)) {
    stop("Probabilities cann't be negative.", .call = FALSE)
  }
  p
}

#' @rdname check_p
#' @export
check_p.ffp <- function(p) {
  p <- as.matrix(as.double(p))
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("Probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.00001)) {
    stop("Probabilities cann't be negative.", .call = FALSE)
  }
  p
}


#' @rdname check_p
#' @export
check_p.matrix <- function(p) {
  p <- matrix(p, ncol = 1)
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("Probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.00001)) {
    stop("Probabilities cann't be negative.", .call = FALSE)
  }
  p
}

#' @rdname check_p
#' @export
check_p.xts <- function(p) {
  p <- matrix(p, ncol = 1)
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("Probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.00001)) {
    stop("Probabilities cann't be negative.", .call = FALSE)
  }
  p
}

#' @rdname check_p
#' @export
check_p.data.frame <- function(p) {
  p <- as.matrix(dplyr::select(p, where(is.numeric)))
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("Probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.00001)) {
    stop("Probabilities cann't be negative.", .call = FALSE)
  }
  p
}


#' @rdname check_p
#' @export
check_p.tbl <- function(p) {
  p <- as.matrix(dplyr::select(p, where(is.numeric)))
  if (sum(p) > 1.00001 | sum(p) < 0.99998) {
    stop("Probabilities must sum 1.", call. = FALSE)
  } else if (any(p < -0.00001)) {
    stop("Probabilities cann't be negative.", .call = FALSE)
  }
  p
}



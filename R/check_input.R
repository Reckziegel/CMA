#' Internal Functions to check the consistency of input arguments.
#'
#' @param x A vector, a matrix, a xts or a tibble object.
#'
#' @return A matrix object.
#' @export
#'
#' @examples
#' #
check_input <- function(x) {
  UseMethod("check_input", x)
}

#' @rdname check_input
#' @export
check_input.default <- function(x) {
  stop("CMA doesn't support the `", class(x), "` yet.", call. = FALSE)
}

#' @rdname check_input
#' @export
check_input.numeric <- function(x) {
  as.matrix(x)
}

#' @rdname check_input
#' @export
check_input.matrix <- function(x) {
  x
}

#' @rdname check_input
#' @export
check_input.xts <- function(x) {
  as.matrix(x)
}

#' @rdname check_input
#' @export
check_input.data.frame <- function(x) {
  as.matrix(dplyr::select(x, where(is.numeric)))
}

#' @rdname check_input
#' @export
check_input.tbl <- function(x) {
  as.matrix(dplyr::select(x, where(is.numeric)))
}

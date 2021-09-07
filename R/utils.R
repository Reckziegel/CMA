
# Sanitize ----------------------------------------------------------------

#' Generic that internally checks input arguments
#'
#' @keywords internal
sanitize <- function(x) {
  UseMethod("sanitize", x)
}

#' @rdname sanitize
#' @keywords internal
sanitize.default <- function(x) {
  stop(paste0("CMA doesn't support the `", class(x), "` yet.", call. = FALSE))
}

#' @rdname sanitize
#' @keywords internal
sanitize.matrix <- function(x) {
  x
}

#' @rdname sanitize
#' @keywords internal
sanitize.ts <- function(x) {
  x <- matrix(x, nrow = NROW(x), ncol = NCOL(x))
  x
}

#' @rdname sanitize
#' @keywords internal
sanitize.xts <- function(x) {
  x <- as.matrix(x)
  x
}

#' @rdname sanitize
#' @keywords internal
sanitize.zoo <- function(x) {
  x <- as.matrix(x)
  x
}

#' @rdname sanitize
#' @keywords internal
sanitize.data.frame <- function(x) {
  x <- dplyr::select(x, where(is.numeric) & where(is.double))
  x <- as.matrix(x)
  x
}

#' @rdname sanitize
#' @keywords internal
sanitize.tbl_df <- function(x) {
  x <- dplyr::select(x, where(is.numeric) & where(is.double))
  x <- as.matrix(x)
  x
}



# Other Utils -------------------------------------------------------------

#' @keywords internal
is_empty <- function(x) length(x) == 0

#' @keywords internal
any_is_date <- function(x) {
  stopifnot(tibble::is_tibble(x) | is.data.frame(x))
  purrr::some(x, lubridate::is.Date)
}

#' @keywords internal
any_is_double <- function(x) {
  stopifnot(tibble::is_tibble(x) | is.data.frame(x))
  any(purrr::map_lgl(x, ~ is.double(.) && is.numeric(.)))
}


#' @keywords internal
which_is_date <- function(x) {
  stopifnot(tibble::is_tibble(x) | is.data.frame(x))
  purrr::detect_index(x, methods::is, "Date")
}

#' @keywords internal
get_date_col <- function(x) {
  stopifnot(tibble::is_tibble(x) | is.data.frame(x))
  dplyr::select(x, where(lubridate::is.Date))
}

#' @keywords internal
get_double_col <- function(x) {
  stopifnot(tibble::is_tibble(x) | is.data.frame(x))
  dplyr::select(x, where(is.double) & where(is.numeric))
}

#' @keywords internal
has_dim <- function(x) !is.null(dim(x))

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

#' @keywords internal
has_colnames <- function(x) !is.null(colnames(x))

#' @keywords internal
has_names <- function(x) !is.null(names(x))

#' @keywords internal
has_rownames <- function(x) !is.null(rownames(x))

#' @keywords internal
not_null <- function(x) !is.null(x)

#' @keywords internal
make_tidy_names <- function(x) paste0("...", 1:NCOL(x))

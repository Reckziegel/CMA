#' Object created to make printing prettier.
#'
#' @param x An object
#' @param ... Additional arguments.
#'
#' @return An objecto of the cma class.
#'
#' @export
new_cma_separation <- function(x, ...) {

  dots <- as.list(...)
  vctrs::new_list_of(
    x          = list(marginal = x, copula = dots$copula),
    ptype      = double(),
    ord_margin = dots$ordered_margin,
    cdf        = dots$ordered_cdf,
    class      = "cma",
    kind       = "separation"
  )

}

#' @rdname new_cma_separation
#' @export
new_cma_to_print <- function(x, ...) {
  tibble::new_tibble(x, nrow = nrow(x), class = "new_cma_to_print", ...)
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.new_cma_to_print <- function(x, ...) {
  crayon::cyan("CMA Decomposition")
}


#' @export
#' @importFrom vctrs obj_print_data
obj_print_data.cma <- function(x, ...) {
  x <- new_cma_to_print(tibble::enframe(x, name = "argument"))
  print(x)
}



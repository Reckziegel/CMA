
#' Generate Marginal Distributions
#'
#' This function takes a fitted object from the \code{cma_fit} class and generate
#' new scenarios for the marginal distributions.
#'
#' @param model An object of the \code{cma_fit} class.
#' @param n An \code{integer} with the number of samples to be generated.
#'
#' @return An S3 \code{list} of the class \code{marginal}.
#' @export
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#'
#' # fit the student t
#' dist <- fit_t(x)
#' dist
#'
#' # generate new margins from the fitted model
#' generate_margins(dist, 10000)
generate_margins <- function(model, n) {
    if (inherits(model, "cma_fit")) {
        x <- ghyp::rghyp(n = n, attributes(model)$ghyp)
        new_marginal(x, model$model)
    } else {
        stop("`model` must be an object of the `cma_fit` class.", call. = FALSE)
    }
}





generate_copulas <- function(...) {

}

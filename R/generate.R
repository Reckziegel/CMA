
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




#' Generate Copulas
#'
#' This function takes a fitted object from the \code{cma_copula} class and generate
#' new scenarios for the copula
#'
#' @param model An object of the \code{cma_copula} class.
#' @param n An \code{integer} with the number of samples to be generated.
#'
#' @return An S3 \code{list} of the class \code{bla bla bla}.
#' @export
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#' colnames(x) <- c("a", "b", "c", "d")
#'
#' # fit normal copula
#' sep  <- cma_separation(x)
#' normcop <- fit_copula_normal(sep)
#'
#' generate_copulas(normcop, 10)
generate_copulas <- function(model, n) {
    if (inherits(model, "cma_copula")) {
        mod <- attributes(model)$model

        if (mod == "claytonCopula") {
            x <- copula::rCopula(
                n      = n,
                copula = copula::claytonCopula(param = model$estimate, dim = model$dimension)
            )
        } else if (mod == "gumbelCopula") {
            x <- copula::rCopula(
                n      = n,
                copula = copula::gumbelCopula(param = model$estimate, dim = model$dimension)
            )
        } else if (mod == "frankCopula") {
            x <- copula::rCopula(
                n      = n,
                copula = copula::frankCopula(param = model$estimate, dim = model$dimension)
            )
        } else if (mod == "joeCopula") {
            x <- copula::rCopula(
                n      = n,
                copula = copula::joeCopula(param = model$estimate, dim = model$dimension)
            )
        } else if (mod == "tCopula") {
            x <- copula::rCopula(
                n      = n,
                copula = copula::tCopula(param = model$estimate, dim = model$dimension)
            )
            # TODO add the df argument form the fitted object
        } else if (mod == "normalCopula") {
            x <- copula::rCopula(
                n      = n,
                copula = copula::normalCopula(param = model$estimate, dim = model$dimension)
            )
        } else {
            stop("model currently not implemeted", call. = FALSE)
        }

        if (!has_colnames(x)) {
            colnames(x) <- make_tidy_names(x)
        }

        tibble::as_tibble(x, .name_repair = "minimal")

    } else {
        stop("`model` must be an object of the `cma_copula` class.", call. = FALSE)
    }

}

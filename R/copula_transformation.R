#' Copula Normalization
#'
#' This function transforms the entries an arbitrary copula so they can be compatible
#' with the first two moments of a dataset.
#'
#' Under the hood, the copula transformation is a 4-step recipe:
#'     \itemize{
#'       \item 1. Normalize the copula to have a standard normal distribution;
#'       \item 2. Operate an affine transformation on the normalized variables;
#'       \item 3. Do the CMA Separation step on the new rotated variables;
#'       \item 4. Extract the copula component that appears in 3.
#'     }
#'
#' @param x A multivariate (non-tidy) dataset.
#' @param copula A multivariate copula.
#'
#' @return A \code{tibble}
#' @export
#'
#' @examples
#' x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
#' sep <- cma_separation(x)
#' clayton <- fit_copula_clayton(sep)
#' gen <- generate_copulas(clayton, 10)
#' copula_transformation(x, gen)
copula_transformation <- function(x, copula) {

    copula <- check_input(copula)
    data   <- check_input(x)

    standardized_dist <- apply(copula, 2, stats::qnorm)
    affinity <- affine_scenarios(
        x     = standardized_dist,
        mu    = colMeans(x),
        sigma = stats::cov(x)
    )

    cma_separation(affinity)$copula

}



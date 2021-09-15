#' Copula Normalization
#'
#' This function transforms the entries of the copula to respect the boundaries
#' of the unit cube.
#'
#' The copula transformation is a 4-step recipe:
#'     \itemize{
#'       \item 1. Generate new copula scenarios;
#'       \item 2. Normalize the copula to have a standard normal distribution;
#'       \item 3. Operate an affine transformation on the normalized variables;
#'       \item 4. Extract the copulas from the new rotated variables.
#'     }
#'
#' @param x A multivariate (non-tidy) dataset.
#' @param copula A multivariate copula.
#'
#' @return A \code{tibble}
#' @export
#'
#' @examples
#' #
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



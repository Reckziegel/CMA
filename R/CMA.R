#' CMA: Copula-Marginal Algorithm
#'
#' The Copula-Marginal Algorithm (CMA) provides a tool to estimate
#' extremely flexible copulas. With CMA, it's possible to extract the copulas and
#' the marginals from any arbitrary joint distributions to perform arbitrary
#' transformations of those extracted copulas and then to glue those transformed
#' copulas back with another set of arbitrary marginal distributions.
#' This flexibility follows from the fact that, unlike traditional approaches to
#' copulas implementation, CMA does not require the explicit computation of
#' marginal cdf and their inverses. As a result, CMA can generate scenarios for
#' many more copulas than the few parametric families used in the traditional approach.
#'
#' @docType package
#' @name CMA
NULL

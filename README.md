
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CMA

<!-- badges: start -->

[![R-CMD-check](https://github.com/Reckziegel/CMA/workflows/R-CMD-check/badge.svg)](https://github.com/Reckziegel/CMA/actions)
[![Codecov test
coverage](https://codecov.io/gh/Reckziegel/CMA/branch/main/graph/badge.svg)](https://codecov.io/gh/Reckziegel/CMA?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Travis build
status](https://travis-ci.com/Reckziegel/CMA.svg?branch=main)](https://travis-ci.com/Reckziegel/CMA)

<!-- badges: end -->

> Multivariate Distribution = Marginals + Copulas

The Copula Marginal Algorithm (CMA) is a simple two step recipe to
manipulate multivariate distributions under [Fully Flexible
Probabilities](https://github.com/Reckziegel/FFP). CMA can quickly
decompose any multivariate distribution between unique information
(*marginals*) and shared components (*copulas*).

This approach can add a high level of flexibility for estimation and
simulation.

For example, let’s suppose `cma_separation()` is used to isolate the
margins from the copulas. Then, the statistician could stress-test any
of these two components and “glue” back the new itens with
`cma_combination()`.

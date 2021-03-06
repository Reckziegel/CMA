
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CMA

<!-- badges: start -->

[![R-CMD-check](https://github.com/Reckziegel/CMA/workflows/R-CMD-check/badge.svg)](https://github.com/Reckziegel/CMA/actions)
[![Codecov test
coverage](https://codecov.io/gh/Reckziegel/CMA/branch/main/graph/badge.svg)](https://codecov.io/gh/Reckziegel/CMA?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

> Multivariate Distribution = Marginals + Copulas

The Copula Marginal Algorithm (CMA) is a simple two step recipe to
manipulate multivariate distributions under [Fully Flexible
Probabilities](https://reckziegel.github.io/FFP/).

CMA can quickly decompose any multivariate distribution between unique
(*marginals*) and their shared components (*copulas*). This approach can
add a high level of flexibility for estimation and simulation purposes.

# Vignettes

-   Panic Copulas:
    [here](https://reckziegel.github.io/CMA/articles/panic_copula.html)

-   “What if” Analysis:
    [here](https://reckziegel.github.io/CMA/articles/what_if_analysis.html)

# Installation

Install the development version of `CMA` from github with:

``` r
# install.packages("devtools")
devtools::install_github("Reckziegel/CMA")
```

# References

-   Meucci, Attilio, A New Breed of Copulas for Risk and Portfolio
    Management (May 22, 2011). Risk, Vol. 24, No. 9, pp. 122-126, 2011,
    Available at SSRN: <https://www.ssrn.com/abstract=1752702>

-   Meucci, Attilio, A Short, Comprehensive, Practical Guide to Copulas
    (May 20, 2011). GARP Risk Professional, p. 22-27, October 2011,
    Available at SSRN: <https://www.ssrn.com/abstract=1847864> or
    <http://dx.doi.org/10.2139/ssrn.1847864>

-   Attilio Meucci (2021). Copula-Marginal Algorithm (CMA)
    (<https://www.mathworks.com/matlabcentral/fileexchange/32701-copula-marginal-algorithm-cma>),
    MATLAB Central File Exchange. Retrieved September 23, 2021.

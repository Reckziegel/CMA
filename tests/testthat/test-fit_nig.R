x_uv <- matrix(stats::rnorm(250), ncol = 1)
x_mv <- matrix(stats::rnorm(500), ncol = 2)

fit_uv <- fit_nig(x_uv)
fit_mv <- fit_nig(x_mv)

test_that("fit_nig works for univariate data", {

    # class
    expect_s3_class(fit_uv, "cma_fit")
    expect_type(fit_uv, "list")
    # size
    expect_length(fit_uv, 21L)
    # attributes
    expect_s4_class(attributes(fit_uv)$ghyp, "ghyp")

})

test_that("fit_nig works for multivariate data", {

    # class
    expect_s3_class(fit_mv, "cma_fit")
    expect_type(fit_mv, "list")
    # size
    expect_length(fit_mv, 21L)
    # attributes
    expect_s4_class(attributes(fit_mv)$ghyp, "ghyp")

})

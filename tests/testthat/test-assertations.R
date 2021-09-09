uv_series <- stats::runif(10)
mv_series <- matrix(stats::runif(20), ncol = 2)

test_that("`assert_is_univariate()`", {
    expect_true(assert_is_univariate(uv_series))
    expect_error(assert_is_univariate(mv_series))
})

test_that("`assert_is_multivariate()`", {
    expect_error(assert_is_multivariate(uv_series))
    expect_true(assert_is_multivariate(mv_series))
})

test_that("`assert_cols_length()`", {
    expect_true(assert_cols_length(mv_series[ , 1], mv_series[ , 2]))
})

test_that("`assert_rows_length()`", {
    expect_true(assert_rows_length(mv_series[ , 1], mv_series[ , 2]))
})

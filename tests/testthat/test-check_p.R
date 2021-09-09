p <- stats::runif(10)
p <- p / sum(p)
index <- seq(Sys.Date(), Sys.Date() + 9, "day")

# data numeric
p_dbl <- p
# matrix data
p_mtx <- matrix(p, ncol = 1)
# data xts
p_xts <- xts::xts(p, order.by = index)
# data data.frame
p_df <- data.frame(index = index, p = p)
# data tbl
p_tbl <- tibble::tibble(index = index, a = p)

test_that("check_p transforms objects into a matrix", {

    # dbl
    expect_type(check_p(p_dbl), "double")
    expect_length(check_p(p_dbl), 10L)
    # mtx
    expect_type(check_p(p_mtx), "double")
    expect_equal(nrow(check_p(p_mtx)), 10L)
    expect_equal(ncol(check_p(p_mtx)), 1L)
    # xts
    expect_type(check_p(p_xts), "double")
    expect_equal(nrow(check_p(p_xts)), 10L)
    expect_equal(ncol(check_p(p_xts)), 1L)
    # data.frame
    expect_type(check_p(p_df), "double")
    expect_equal(nrow(check_p(p_df)), 10L)
    expect_equal(ncol(check_p(p_df)), 1L)
    # tbl
    expect_type(check_p(p_tbl), "double")
    expect_equal(nrow(check_p(p_tbl)), 10L)
    expect_equal(ncol(check_p(p_tbl)), 1L)

})


test_that("ts throws and error", {
  expect_error(check_input(AirPassengers / sum(AirPassengers)))
})

p_negative <- runif(1000, -1, 1)
p_negative <- p_negative / sum(p_negative)
test_that("negative numbers are rejected",{
  expect_error(check_p(p_negative))
})

p_above_one <- runif(100, 0, 100)
test_that("negative numbers are rejected",{
  expect_error(check_p(p_above_one))
})


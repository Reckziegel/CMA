ret <- diff(log(EuStockMarkets))
p <- rep(1 / nrow(ret), nrow(ret))
emp <- empirical_stats(ret, p)

test_that("empirical_stats()  works", {
  expect_s3_class(emp, "tbl")
  expect_equal(ncol(emp), 3L)
  expect_equal(nrow(emp), 24L)
})

test_that("empirical_stats() works when p = NULL", {
  expect_equal(emp, empirical_stats(ret))
})

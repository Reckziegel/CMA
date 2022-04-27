x <- diff(log(EuStockMarkets))
pc <- panic_copula(x = x, n = 20, panic_cor = 0.99, panic_prob = 0.02)

test_that("panic_copula works", {
  # type
  expect_type(pc, "list")
  expect_s3_class(pc, "panic_copula")
  # size
  expect_length(pc, 2L)

  # objects in `pc` list have the right format and size
  expect_s3_class(pc$simulation, "tbl_df")
  expect_equal(nrow(pc$simulation), 20L)
  expect_equal(ncol(pc$simulation), 4L)

  expect_s3_class(pc$p, "ffp")

})

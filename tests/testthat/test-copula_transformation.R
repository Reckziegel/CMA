x       <- matrix(diff(log(EuStockMarkets)), ncol = 4)
sep     <- cma_separation(x)
clayton <- fit_copula_clayton(sep)
gen     <- generate_copulas(clayton, 10)
cop_trans <- copula_transformation(x, gen)

test_that("copula_transformation works", {
  # type
  expect_type(cop_trans, "list")
  expect_s3_class(cop_trans, "tbl_df")
  # size
  expect_equal(ncol(cop_trans), 4L)
  expect_equal(nrow(cop_trans), 10L)
  # names
  #expect_named(comb, c("a", "b"))

})

x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
sep <- cma_separation(x)

clayton <- fit_copula_clayton(sep)
gumbel  <- fit_copula_gumbel(sep)
frank   <- fit_copula_frank(sep)
t       <- suppressWarnings(fit_copula_t(sep))
normal  <- fit_copula_normal(sep)
joe     <- fit_copula_joe(sep)

test_that("fit_copula_* works", {

  # Clayton
  expect_s3_class(clayton, "cma_copula")
  expect_length(clayton, 9L)

  # Gumbel
  expect_s3_class(gumbel, "cma_copula")
  expect_length(gumbel, 9L)

  # frank
  expect_s3_class(frank, "cma_copula")
  expect_length(frank, 9L)

  # t
  expect_s3_class(t, "cma_copula")
  expect_length(t, 9L)

  # normal
  expect_s3_class(normal, "cma_copula")
  expect_length(normal, 9L)

  # joe
  expect_s3_class(joe, "cma_copula")
  expect_length(joe, 9L)

})

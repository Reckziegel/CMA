margins <- matrix(stats::rnorm(20), ncol = 2)
colnames(margins) <- c("a", "b")
margins <- tibble::as_tibble(margins)
probs   <- rep(1 / 10, 10)
# separate
sep <- cma_separation(x = margins, p = probs)
# combinate
comb <- cma_combination(margins, sep$cdf, sep$copula)
all.equal(margins[ , "a"], comb$a)
all.equal(margins[ , "b"], comb$b)

test_that("cma_combination works", {
  # type
  expect_type(comb, "list")
  expect_s3_class(comb, "tbl_df")
  # size
  expect_equal(ncol(comb), 2L)
  expect_equal(nrow(comb), 10L)
  # names
  expect_named(comb, c("a", "b"))
  # identity
  expect_equal(margins, comb)
})

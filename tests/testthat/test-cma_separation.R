data <- stats::runif(20)
index <- seq(Sys.Date(), Sys.Date() + 9, "day")
p <- stats::runif(10)
p <- p / sum(p)
# data numeric
data_dbl <- data
# matrix data
data_mtx <- matrix(data, ncol = 2)
# data xts
data_xts <- xts::xts(matrix(data, ncol = 2), order.by = index)
# data data.frame
data_df <- data.frame(matrix(data, ncol = 2))
# data tbl
data_tbl <- tibble::tibble(index = index, a = data[1:10], b = data[11:20])

# Flexible probabilities
cma_sep_mtx <- cma_separation(x = data_mtx, p = p)
cma_sep_xts <- cma_separation(x = data_xts, p = p)
cma_sep_df  <- cma_separation(x = data_df,  p = p)
cma_sep_tbl <- cma_separation(x = data_tbl, p = p)

# equal probabilites
cma_sep_mtx_ep <- cma_separation(x = data_mtx)
cma_sep_xts_ep <- cma_separation(x = data_xts)
cma_sep_df_ep  <- cma_separation(x = data_df)
cma_sep_tbl_ep <- cma_separation(x = data_tbl)

test_that("cma_separation works on matrices", {
    expect_type(cma_sep_mtx, "list")
    expect_s3_class(cma_sep_mtx, "cma")
    expect_length(cma_sep_mtx, 2L)
    expect_named(cma_sep_mtx, c("marginal", "copula"))

    expect_type(cma_sep_mtx_ep, "list")
    expect_s3_class(cma_sep_mtx_ep, "cma")
    expect_length(cma_sep_mtx_ep, 2L)
    expect_named(cma_sep_mtx_ep, c("marginal", "copula"))
})

test_that("cma_separation works on xts", {
    expect_type(cma_sep_xts, "list")
    expect_s3_class(cma_sep_xts, "cma")
    expect_length(cma_sep_xts, 2L)
    expect_named(cma_sep_xts, c("marginal", "copula"))

    expect_type(cma_sep_xts_ep, "list")
    expect_s3_class(cma_sep_xts_ep, "cma")
    expect_length(cma_sep_xts_ep, 2L)
    expect_named(cma_sep_xts_ep, c("marginal", "copula"))
})

test_that("cma_separation works on data.frames", {
    expect_type(cma_sep_df, "list")
    expect_s3_class(cma_sep_df, "cma")
    expect_length(cma_sep_df, 2L)
    expect_named(cma_sep_df, c("marginal", "copula"))

    expect_type(cma_sep_df_ep, "list")
    expect_s3_class(cma_sep_df_ep, "cma")
    expect_length(cma_sep_df_ep, 2L)
    expect_named(cma_sep_df_ep, c("marginal", "copula"))
})

test_that("cma_separation works on tbl", {
    expect_type(cma_sep_tbl, "list")
    expect_s3_class(cma_sep_tbl, "cma")
    expect_length(cma_sep_tbl, 2L)
    expect_named(cma_sep_tbl, c("marginal", "copula"))

    expect_type(cma_sep_tbl_ep, "list")
    expect_s3_class(cma_sep_tbl_ep, "cma")
    expect_length(cma_sep_tbl_ep, 2L)
    expect_named(cma_sep_tbl_ep, c("marginal", "copula"))
})

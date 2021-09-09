#' @keywords internal
solve_riccati <- function(sigma) {

    n_row <- nrow(sigma)
    zero_matrix <- matrix(0, n_row, n_row)
    diag_matrix <- diag(n_row)

    h <- cbind(
        rbind(zero_matrix, -sigma),
        rbind(-diag_matrix, zero_matrix)
    )

    # schur decomposition
    schur <- QZ::ordqz(h, keyword = "lhp")
    u <- schur$Q
    u11 <- u[1:n_row, 1:n_row]
    u21 <- u[(n_row + 1):(2 * n_row), 1:n_row]

    # riccati solution
    b <- u21 %*% solve(u11)
    b

}

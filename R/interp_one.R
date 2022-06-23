#' @keywords internal
interp_one <- function(x, y, xi = x) {

    if (!is.vector(x, mode = "numeric") || !is.vector(y, mode = "numeric")) {
        stop("Arguments 'x' and 'y' must be numeric vectors.")
    }

    nx <- length(x)

    if (length(y) != nx) {
        stop("Arguments 'x' and 'y' must be vectors of the same length.")
    }

    if (nx <= 1) {
        stop("Arguments 'x', 'y' must have at least a length >= 2.")
    }

    if (min(xi) < min(x) || max(xi) > max(x)) {
        stop("Points 'xi' outside of range of argument 'x'.")
    }

    if (is.unsorted(x)) {
        #warning("Points in argument in 'x' unsorted; will be sorted.")
        o <- order(x)
        x <- x[o]
        y <- y[o]
    }

    # FIXME does duplicated.default improves performance?
    if (any(duplicated(x))) {
        warning("There are duplicated values in 'x'; mean will be tried.")
    }

    yi <- stats::approx(x, y, xi, method = "linear")$y
    yi

}

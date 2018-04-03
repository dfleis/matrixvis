#' @title Draw an image plot of a matrix.
#'
#' @description This function displays an image of a matrix via
#' the R image function so that the image is oriented in the same
#' way as the matrix output. For example, if \eqn{X} is a matrix
#' with entries \eqn{X_{i,j}, i = 1, ..., n, j = 1, ..., m}, then
#' \eqn{X_{1,1}} will be displayed at the top-left, \eqn{X_{n, 1}}
#' the bottom-left, \eqn{X_{1, m}} the top-right, and \eqn{X_{n, m}}
#' the bottom-right.
#'
#' @param ... The expected arguments \code{x}, \code{y}, and \code{z}
#' of the \code{graphics::image} function.
#'
#' @export
#'
#' @examples
#' n <- 10
#' p <- 7
#' M <- matrix(rnorm(n * p), nrow = n)
#' image(M)
#'
matrix_image <- function(x, y, z, col = NULL, ncol = 64, ...) {
  if (is.null(col)) {
    col <- viridis::viridis_pal(option = "D")(ncol)
  }
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      } else {
        if (is.null(dim(x))) {
          stop("No matrix or list input provided.")
        }
        z <- x
        x <- seq_len(nrow(z))
        y <- seq_len(ncol(z))
      }
    } else {
      stop("No matrix provided.")
    }
  } else {
    if (missing(x)) {
      x <- seq_len(nrow(z))
    }
    if (missing(y)) {
      y <- seq_len(ncol(z))
    }
  }
  z <- t(apply(z, 2, rev))

  image(x = y, y = x, z = z, col = col, ...)
}

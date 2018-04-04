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
#' @param x A vector of length \code{ncol(z)} corresponding to the column
#'          indices of \code{z}. By default \code{x} is set to the integer
#'          sequence 1, 2, ...,  \code{ncol(z)}. Passed to the
#'          \code{graphics::image} parameter \code{x}.
#' @param y A vector of length \code{nrow(z)} corresponding to the row
#'          indices of \code{z}. By default{y} is set to the integer
#'          sequence 1, 2, ..., \code{nrow(z)}. Passed to the
#'          \code{graphics::image} parameter \code{x}.
#' @param z A numeric or logical matrix to be plotted. Elements with values
#'          \code{NA}, \code{NaN}, or \code{Inf} are permitted (as
#'          long as not all elements of \code{z} are \code{NA}, \code{NaN},
#'          \code{Inf}) but will be drawn as blank cells. Note that \code{x} can be
#'          used as \code{z} if \code{z} not specified (but the values
#'          passed to \code{y} will be ignored).
#' @param {xlab, ylab} Character strings corresponding to the labels
#'                     for the \eqn{x} and \eqn{y} axes. Default labels
#'                     are set to \code{"Columns"} and \code{"Rows"} for
#'                     the \eqn{x} and \eqn{y} axes, respectively.
#'
#' @param ... Other arguments to be passed to \code{graphics::image}.
#'
#' @export
#'
#' @examples
#' n <- 10
#' p <- 7
#' M <- matrix(rnorm(n * p), nrow = n)
#' matrix_image(M)
#'
matrix_image <- function(x, y, z,
                         xlab, ylab,
                         xaxt, yaxt,
                         col, ncol = 64, ...) {
  if (missing(col)) col <- viridis::viridis_pal(option = "D")(ncol)
  if (missing(xlab)) xlab <- "Columns"
  if (missing(ylab)) ylab <- "Rows"

  if (missing(z)) {
    if (!missing(x)) {
      z <- x; n <- nrow(z); p <- ncol(z)
      x <- seq_len(p)
      y <- seq_len(n)
    } else {
      stop("No matrix provided.")
    }
  } else {
    n <- nrow(z); p <- ncol(z)

    if (missing(x)) x <- seq_len(p) # columns
    if (missing(y)) y <- seq_len(n) # rows
  }

  if (missing(xaxt)) xaxt.out <- "n"
  if (missing(yaxt)) yaxt.out <- "n"

  z <- t(apply(z, 2, rev))

  image(x = x, y = y, z = z, col = col,
        xlab = xlab,
        ylab = ylab,
        xaxt = xaxt.out,
        yaxt = yaxt.out, ...)
  if (missing(xaxt)) {
    if (p > 5) xax <- pretty(1:p) else xax <- (1:p)
    axis(1, at = xax, labels = xax)
  }
  if (missing(yaxt)) {
    if (n > 5) yax <- pretty(1:n) else yax <- (1:n)
    axis(2, at = yax, labels = yax)
  }

}









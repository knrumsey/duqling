#' The Foursquare Function
#'
#' Dimensions: 2
#'
#' @param fname function name (from duqling)
#' @param nx grid size (one side)
#' @param ncol number of colors used in plot
#' @param pal palette passed to hcl.colors
#' @param dims length must be 2. Which dimensions should be plotted?
#' @return A figure
#' @details Plots 2 dimensions of a specified test functions
#' @export
#' @examples
#' show_function_2d("twin_galaxies")
#' show_function_2d("borehole", dims=c(1,6))
show_function_2d <- function(fname, nx=201, ncol=100, pal="terrain", dims=1:2, ...){
  xx <- seq(0, 1, length=nx)
  XX <- expand.grid(xx, xx)
  XX2 <- matrix(0.5, nrow=nrow(XX), ncol=quack(fname)$input_dim)
  XX2[,dims[1]] <- XX[,1]
  XX2[,dims[2]] <- XX[,2]
  yy <- duq(XX2, fname, scale01=TRUE, ...)
  YY <- matrix(yy, nrow=nx, ncol=nx)
  image(YY, col=hcl.colors(ncol, pal))
}

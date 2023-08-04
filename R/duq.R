#' duq
#'
#' Call functions from duqling namespace
#'
#' @param x An nxp matrix of inputs.
#' @param f A function, usually from the duqling package
#' @param ... additional functions to be passed to f
#' @return See details
#' @details If fname is specified, this function returns a list with the number of input dimensions and a p x 2 matrix of input ranges. If fname is not specified, then `quackquack` returns a list of function names which satisfy the requirements specified by the other inputs. If no arguments are specified, then a list of all functions is returned.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#' @export
#' @examples
#' n <- 100
#' p <- 10
#' X <- lhs::randomLHS(n, p)
#' duq(X, borehole)
duq <- function(x, f, ...){
  if(is.null(nrow(x))){
    x <- matrix(x, nrow=1)
  }
  apply(x, 1, f, ...)
}

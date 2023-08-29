#' duq
#'
#' Call functions from duqling namespace
#'
#' @param x An nxp matrix of inputs.
#' @param f A string (of a function name) or a function, usually from the duqling package
#' @param scale01 When TRUE, inputs are expected on the (0, 1) scale.
#' @param ... additional functions to be passed to f
#' @return See details
#' @details A shortcut for calling funtions in the duqling package
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#' @export
#' @examples
#' n <- 100
#' p <- 10
#' X <- lhs::randomLHS(n, p)
#' duq(X, borehole)
duq <- function(x, f, scale01=TRUE, ...){
  if(is.null(nrow(x))){
    x <- matrix(x, nrow=1)
  }
  if(is.character(f)){
    f <- get(f)
  }
  apply(x, 1, f, ...)
}

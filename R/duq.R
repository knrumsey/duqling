#' @title Evaluate a UQ Test Function
#'
#' @description Call functions from duqling namespace
#'
#' @param f A string (of a function name) or a function, usually from the duqling package
#' @param x A matrix of categorical inputs with \code{n} rows and \code{quack()$input_dim} columns.
#' @param z A matrix of categorical inputs with \code{n} rows and \code{quack()$input_cat_dim} columns. Categories should be coercible to an integer between 1 and number of levels.
#' @param scale01 When \code{TRUE}, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param ... additional functions to be passed to f
#' @return See details
#' @details A shortcut for calling funtions in the duqling package
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#' @export
#' @examples
#' n <- 100
#' p <- 2
#' X <- lhs::randomLHS(n, p)
#' f_list <- quack(input_dim=2)$fname
#' eval_duq(f_list[1], X)
eval_duq <- function(f, x, z=NULL, scale01=TRUE, ...){
  if(is.null(nrow(x))){
    x <- matrix(x, ncol=1)
  }
  if(is.null(nrow(z))){
    z <- matrix(z, ncol=1)
  }
  has_cat <- FALSE
  if(is.character(f)){
    qq <- quack(f, verbose=FALSE)
    has_cat <- qq$has_categorical
    # Look for f in duqling first, then just use local environment
    f <- tryCatch(
      get(f, envir = asNamespace("duqling")),
      error = function(e) get(f, envir = parent.frame())
    )
  }
  if(!has_cat){
    res <- apply(x, 1, f, scale01=scale01, ...)
  }else{
    if(qq$response == "univariate"){
      res <- vapply(1:nrow(x), function(i) f(x[i,], z[i,], scale01, ...), 1)
    }else{
      test <- f(x[1,], z[1,], scale01, code01, ...)
      res <- vapply(1:nrow(x), function(i) f(x[i,], z[i,], scale01, ...), test)
    }
  }
  return(res)
}

#' @export
duq <- eval_duq

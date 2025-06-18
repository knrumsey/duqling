#' @title Sobol Function
#'
#' @param x Inputs of dimension (at least) 3. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval.
#' @details Often used as an example able to compare sobol indices directly
#' @references
#' I.M. Sobol’, Sensitivity estimates for nonlinear mathematical models, Math. Model. Comput. Exp. 1 (1993) 407–414.
#' @rdname sobol
#' @examples
#' X <- lhs::randomLHS(50, 9)
#' y <- apply(X, 1, sobol, scale01=TRUE)
#' @export
sobol <- function(x, scale01=TRUE){
  res <- 1
  for (i in 1:length(x)){
    b <- (i-1) / 4
    res <- res * (abs(4*x[i]-2)+b)/(1+b)
  }
  return(res)
}


quackquack_sobol <- function(){
  out <- list(input_dim=3)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  out$stochastic <- "n"

  RR <- cbind(rep(0, 3), rep(1, 3))
  rownames(RR) <- c("x1", "x2", "x3")
  out$input_range <- RR
  return(out)
}

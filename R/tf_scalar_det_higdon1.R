#' 1D Higdon Function
#'
#' Dimensions: 1. This function is a simple one-dimensional test function exhibiting non-stationarity.
#'
#' @param x Inputs of dimension (at least) 1.
#' @param scale01 Are inputs expected on (0, 1) scale?.
#' @return Scalar output
#' @details A simple 1D test function. Low fidelity version is simulated as Af(x) + B(x-1/2) - C. See reference for details.
#' @references
#'
#' Gramacy, R.B., Lee, H.K. and Macready, W.G., 2004, July. Parameter space exploration with Gaussian process trees. In Proceedings of the twenty-first international conference on Machine learning (p. 45)
#'
#' @export
#' @examples
#' fname <- "higdon1"
#' n <- 10
#' p <- quack(fname, verbose=FALSE)$input_dim
#' x <- matrix(runif(n*p), nrow=n)
#' y <- eval_duq(fname, x)
higdon1 <- function(x, scale01=TRUE){
  if(scale01){
    RR <- cbind(0, 50)

    x[1] <- x[1]*(RR[,2] - RR[,1]) + RR[,1]
  }
  term1 <- (sin(pi * x[1] / 5) + (1/5) * cos(4 * pi * x[1] / 5)) * as.numeric(x[1] >= 35.75)
  term2 <- (1 - x[1] / 35.75) * as.numeric(x[1] < 35.75)
  term1 + term2
}


quackquack_higdon1 <- function(){
  out <- list(input_dim=1)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(0, 50)
  rownames(RR) <- c("x")
  out$input_range <- RR

  return(out)
}

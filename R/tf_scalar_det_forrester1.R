#' 1D Multifidelity Function
#'
#' Dimensions: 1. This function is a simple one-dimensional test function. It is multimodal, with one global minimum, one local minimum and a zero-gradient inflection point.
#'
#' @param x Inputs of dimension (at least) 1.
#' @param scale01 Ignored.
#' @param A Parameter for low fidelity version (see details)
#' @param B Parameter for low fidelity version (see details)
#' @param C Parameter for low fidelity version (see details)
#' @return Scalar output
#' @details A simple 1D test function. Low fidelity version is simulated as Af(x) + B(x-1/2) - C. See reference for details.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Forrester, A., Sobester, A., & Keane, A. (2008). Engineering design via surrogate modelling: a practical guide. Wiley.
#'
#' @export
#' @examples
#' n <- 1
#' x <- matrix(runif(20), nrow=n)
#' y <- apply(x, 1, forrester1)
forrester1 <- function(x, scale01=TRUE, A=1, B=0, C=0){
  fx <- (6*x[1] - 0.5)^2 * sin(12*x[1] - 4)
  res <- A*fx + B*(x - 0.5) - C
}

#' @export
forrester1_low_fidelity <- function(x, scale01=TRUE){
  forrester1(x, A=0.5, B=10, C=-5)
}

quackquack_forrester1 <- function(){
  out <- list(input_dim=1)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(0,1)
  rownames(RR) <- c("x")
  out$input_range <- RR

  return(out)
}

quackquack_forrester1_low_fidelity <- function(){
  out <- list(input_dim=1)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(0,1)
  rownames(RR) <- c("x")
  out$input_range <- RR

  return(out)
}

#' @name park
#' @rdname park
#'
#' @title The Park Function
#'
#' @description
#' Dimensions: 4.
#'
#' @param x Inputs of dimension (at least) 3. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval.
#' @param ab Hyperparameters to the park function
#' @details Intended to be a simple four-dim test function. Xiong et al. (2013) use \code{park4_low_fidelity} as a low fidelity version. For details on the Friedman function, see \href{https://www.sfu.ca/~ssurjano/park91a.html}{the VLSE}.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Park, Jeong Soo. Tuning complex computer codes to data and optimal designs. University of Illinois at Urbana-Champaign, 1991.
#'
#' Xiong, Shifeng, Peter ZG Qian, and CF Jeff Wu. "Sequential design and analysis of high-accuracy and low-accuracy computer codes." Technometrics 55.1 (2013): 37-46.
NULL

#' @rdname park
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, friedman, scale01=TRUE)
park4 <- function(x, scale01=TRUE){
  term1 <- x[1]/2*(sqrt(1+(x[2]+x[3]^2)*x[4]/x[1]^2) - 1)
  term2 <- (x[1] + 3*x[4]) * exp(1 + sin(x[3]))
  res <- term1 + term2
  return(res)
}

#' @rdname park
#' @export
park4_low_fidelity <- function(x, scale01=TRUE){
  term1 <- x[1]/2*(sqrt(1+(x[2]+x[3]^2)*x[4]/x[1]^2) - 1)
  term2 <- (x[1] + 3*x[4]) * exp(1 + sin(x[3]))
  term3 <- -2*x[1] + x[2]^2 + x[3]^2 + 1/2
  res <- (1 + sin(x[1])/10)*(term1 + term2) + term3
  return(res)
}


quackquack_park4 <- function(){
  out <- list(input_dim=4)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  out$stochastic <- "n"

  RR <- cbind(rep(0, 4), rep(1, 4))
  rownames(RR) <- c("x1", "x2", "x3", "x4")
  out$input_range <- RR
  return(out)
}

quackquack_park4_low_fidelity <- function(){
  out <- list(input_dim=4)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  out$stochastic <- "n"

  RR <- cbind(rep(0, 4), rep(1, 4))
  rownames(RR) <- c("x1", "x2", "x3", "x4")
  out$input_range <- RR
  return(out)
}

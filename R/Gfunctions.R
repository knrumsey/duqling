#' @name Gfunction
#' @rdname Gfunction
#'
#' @title The Gfunction Function
#' @description
#' Dimensions: d. A multiplicative function.
#'
#' @param x Inputs of dimension d. See below for details.
#' @param scale01 (Ignored here) When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param a Weight vector of length d (lower value is more important)
#' @return A scalar response
#' @details The G function function is often used as an integrand for various numerical estimation methods. The exact value of the intregal of this function in any dimensions is 1. The a_i values indicate the importance of a variable (lower value is more important).
#' \code{Gfunction} takes a 3-dimensional input vector by default but accepts any number of dimension with input i having weight (i-1)/4. \code{Gfunction6} forces all inputs after the p=6th to be inert (similarly for \code{Gfunction12} and \code{Gfunction18} with p=12 and p=18).
#' For details on the G function function, see \href{https://www.sfu.ca/~ssurjano/fried.html}{the VLSE}.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Saltelli, A., & Sobol', I. M. (1995). About the use of rank transformation in sensitivity analysis of model output. Reliability Engineering & System Safety, 50(3), 225-239.
#'
#' Marrel, A., Iooss, B., Van Dorpe, F., & Volkova, E. (2008). An efficient methodology for modeling complex computer codes with Gaussian processes. Computational Statistics & Data Analysis, 52(10), 4731-4744.
NULL


#' @rdname Gfunction
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, Gfunction, scale01=TRUE)
Gfunction <- function(x, scale01=TRUE, a=NULL){
  if(is.null(a)){
    p <- length(x)
    a <- (1:p - 1) / 4
  }
  u <- (abs(4*x - 2) + a)/(1+a)
  res <- prod(u)
}

#' @rdname Gfunction
#' @export
Gfunction6 <- function(x, scale01=TRUE, a=c(0,0,rep(6.52, 4))){
  x <- x[1:6]
  u <- (abs(4*x - 2) + a)/(1+a)
  res <- prod(u)
}

#' @rdname Gfunction
#' @export
Gfunction12 <- function(x, scale01=TRUE, a=c(0,0,rep(6.52,4),9,9,15,25,50,99)){
  x <- x[1:12]
  u <- (abs(4*x - 2) + a)/(1+a)
  res <- prod(u)
}

#' @rdname Gfunction
#' @export
Gfunction18 <- function(x, scale01=TRUE, a=c(rep(0,4), rep(1,4), rep(9,4), rep(18,4), rep(99,2))){
  x <- x[1:18]
  u <- (abs(4*x - 2) + a)/(1+a)
  res <- prod(u)
}



quackquack_Gfunction <- function(){
  out <- list(input_dim=3)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(rep(0, 3),
              rep(1, 3))
  rownames(RR) <- c("x1", "x2", "x3")
  out$input_range <- RR
  return(out)
}

quackquack_Gfunction6 <- function(){
  out <- list(input_dim=6)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(rep(0, 6),
              rep(1, 6))
  rownames(RR) <- c("x1", "x2", "x3", "x4", "x5", "x6")
  out$input_range <- RR
  return(out)
}

quackquack_Gfunction12 <- function(){
  out <- list(input_dim=12)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(rep(0, 12),
              rep(1, 12))
  rownames(RR) <- unlist(lapply(1:12,
                                function(zz) paste0("x", zz)))
  out$input_range <- RR
  return(out)
}

quackquack_Gfunction18 <- function(){
  out <- list(input_dim=18)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(rep(0, 18),
              rep(1, 18))
  rownames(RR) <- unlist(lapply(1:18,
                                function(zz) paste0("x", zz)))
  out$input_range <- RR
  return(out)
}

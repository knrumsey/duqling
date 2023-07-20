#' @name grlee
#' @rdname grlee
#'
#' @title Gramacy and Lee Test Functions
#'
#' @param x Inputs of dimension (at least) 8. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @details Functions from three Gramacy and Lee papers. For details, see \href{https://www.sfu.ca/~ssurjano/emulat.html}{the VLSE} (\href{https://www.sfu.ca/~ssurjano/grlee12.html}{grlee1()}, \href{https://www.sfu.ca/~ssurjano/grlee08.html}{grlee2()}, \href{https://www.sfu.ca/~ssurjano/grlee09.html}{grlee6()}) .
#'
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Gramacy, R. B., & Lee, H. K. (2008). Gaussian processes and limiting linear models. Computational Statistics & Data Analysis, 53(1), 123-136.
#'
#' Gramacy, R. B., & Lee, H. K. (2009). Adaptive design and analysis of supercomputer experiments. Technometrics, 51(2).
#'
#' Gramacy, R. B., & Lee, H. K. (2012). Cases for the nugget in modeling computer experiments. Statistics and Computing, 22(3), 713-722.
NULL

#' @rdname grlee
#' @examples
#' n <- 100 #Number of observations
#' p <- 6   #Number of variables
#' X <- matrix(runif(n*p), nrow=n)
#' y <- apply(X, 1, grlee6)
#' @export
grlee6 <- function(x, scale01=FALSE){
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  x4 <- x[4]
  x5 <- x[5]
  x6 <- x[6]

  term1 <- exp(sin((0.9*(x1+0.48))^10))
  term2 <- x2 * x3
  term3 <- x4

  y <- term1 + term2 + term3
  return(y)
}

#' @rdname grlee
#' @export
grlee1 <- function(x, scale01=FALSE){
  if(scale01){
    x <- 0.5 + 2*x[1]
  }
  term1 <- sin(10*pi*x) / (2*x)
  term2 <- (x-1)^4

  y <- term1 + term2
  return(y)
  }

#' @rdname grlee
#' @export
grlee2 <- function(x, scale01=FALSE){
  if(scale01){
    x[1:2] <- 8*x[1:2] - 2
  }
  y = x[1]*exp(-x[1]^2-x[2]^2)
  return(y)
}


quackquack_grlee1 <- function(){
  out <- list(input_dim=1)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(0,
              1)
  rownames(RR) <- c("x")
  out$input_range <- RR

  return(out)
}

quackquack_grlee2 <- function(){
  out <- list(input_dim=2)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(-2, 2),
              c(6, 6))
  rownames(RR) <- c("x1", "x2")
  out$input_range <- RR

  return(out)
}

quackquack_grlee6 <- function(){
  out <- list(input_dim=6)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(rep(0, 6),
              rep(1, 6))
  rownames(RR) <- c("x1", "x2", "x3", "x4", "x5", "x6")
  out$input_range <- RR

  return(out)
}




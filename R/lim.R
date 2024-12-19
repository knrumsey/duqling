#' @name lim
#' @rdname lim
#'
#' @title Lim et al Test Functions
#'
#' @param x Inputs of dimension (at least) 8. See below for details.
#' @param scale01  When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @details Two similar functions from Lim et al (2002). One is a polynomial the other is a non-polynomial but both functions have similar shape and behavior. For details, see \href{https://www.sfu.ca/~ssurjano/limetalpol.html}{the VLSE}.
#'
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Lim, Y. B., Sacks, J., Studden, W. J., & Welch, W. J. (2002). Design and analysis of computer experiments when the output is highly correlated over the input space. Canadian Journal of Statistics, 30(1), 109-126.
NULL

#' @rdname lim
#' @examples
#' n <- 100 #Number of observations
#' p <- 6   #Number of variables
#' X <- matrix(runif(n*p), nrow=n)
#' y <- apply(X, 1, lim_polynomial)
#' @export
lim_polynomial <- function(x, scale01=TRUE){
  x1 <- x[1]
  x2 <- x[2]

  term1 <- (5/2)*x1 - (35/2)*x2
  term2 <- (5/2)*x1*x2 + 19*x2^2
  term3 <- -(15/2)*x1^3 - (5/2)*x1*x2^2
  term4 <- -(11/2)*x2^4 + (x1^3)*(x2^2)

  y <- 9 + term1 + term2 + term3 + term4
  return(y)
}

#' @rdname lim
#' @export
lim_non_polynomial <- function(x, scale01=TRUE){
  x1 <- x[1]
  x2 <- x[2]

  fact1 <- 30 + 5*x1*sin(5*x1)
  fact2 <- 4 + exp(-5*x2)

  y <- (fact1*fact2 - 100) / 6
  return(y)
}


quackquack_lim_polynomial <- function(){
  out <- list(input_dim=2)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(0,0), c(1,1))
  rownames(RR) <- c("x1", "x2")
  out$input_range <- RR

  return(out)
}
quackquack_lim_non_polynomial <- quackquack_lim_polynomial

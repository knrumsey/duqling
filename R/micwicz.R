#' The Multivalley Function
#'
#' Dimension: Default is 2, but can be any dimension. For inert variables, \code{active_dim} must be specified.
#'
#' @param x Inputs of dimension (at least) 2. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param m A larger m leads to a more difficult search (steeper valleys/ridges).
#' @param active_dim Only the first \code{min(active_dim, length(x))} variables will be used. All other variables are inert.
#' @return Function output
#' @details Also called the Michalewicz function. For details on this function, see \href{https://www.sfu.ca/~ssurjano/michal.html}{the VLSE}.
#'
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Molga, M., & Smutnicki, C. Test functions for optimization needs (2005). Retrieved June 2013, from http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf.
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, multivalley, scale01=TRUE)
multivalley <- function(x, scale01=TRUE, m=10, active_dim=length(x)){
  p <- min(active_dim, length(x))
  if(scale01){
    x <- 2*(base::pi)*x
  }

  y <- 0
  for(i in 1:p){
    y <- y - sin(x[i])*sin(i*x[i]^2/(base::pi))^(2*m)
  }
  return(y)
}


quackquack_multivalley <- function(){
  out <- list(input_dim=2)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(0, 0), c(2, 2)*(base::pi))
  rownames(RR) <- c("x1", "x2")
  out$input_range <- RR
  return(out)
}

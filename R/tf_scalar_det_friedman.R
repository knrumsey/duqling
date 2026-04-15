#' @name friedman
#' @rdname friedman
#'
#' @title The Friedman Function
#' @description
#' Dimensions: 5. The Friedman function is often used with additional, inert inputs.
#'
#' @param x Inputs of dimension (at least) 5. See below for details.
#' @param scale01 (Ignored here) When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @return A scalar response
#' @details For details on the Friedman function, see \href{https://www.sfu.ca/~ssurjano/fried.html}{the VLSE}.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Friedman, J. H. (1991). Multivariate adaptive regression splines. The annals of statistics, 19(1), 1-67.
NULL


#' @rdname friedman
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, friedman, scale01=TRUE)
friedman <- function(x, scale01=TRUE){
  10*sin(pi*x[1]*x[2]) + 20*(x[3]-0.5)^2 + 10*x[4] + 5*x[5]
}

#' @rdname friedman
#' @export
friedman10 <- function(x, scale01=TRUE){
  friedman(x)
}

#' @rdname friedman
#' @export
friedman20 <- function(x, scale01=TRUE){
  friedman(x)
}

quackquack_friedman <- function(){
  out <- list(input_dim=5)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(rep(0, 5),
              rep(1, 5))
  rownames(RR) <- c("x1", "x2", "x3", "x4", "x5")
  out$input_range <- RR
  return(out)
}

quackquack_friedman10 <- function(){
  out <- list(input_dim=10)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(rep(0, 10),
              rep(1, 10))
  rownames(RR) <- unlist(lapply(1:10,
                                function(zz) paste0("x", zz)))
  out$input_range <- RR
  return(out)
}

quackquack_friedman20 <- function(){
  out <- list(input_dim=20)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(rep(0, 20),
              rep(1, 20))
  rownames(RR) <- unlist(lapply(1:20,
                                function(zz) paste0("x", zz)))
  out$input_range <- RR
  return(out)
}

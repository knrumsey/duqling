#' The Dette & Pep Curved Function
#'
#' Dimensions 3.
#' @param x Inputs of dimension (at least) 3. See below for details.
#' @param scale01 (No effect) When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @return Response.
#' @details This highly curved function is used for the comparison of computer experiment designs.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Dette, Holger, and Andrey Pepelyshev. "Generalized Latin hypercube design for computer experiments." Technometrics 52.4 (2010): 421-429.
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(3*n), nrow=n)
#' y <- apply(x, 1, detpep_curve, scale01=TRUE)
detpep_curve <- function(x, scale01=TRUE){
  ya <- 4*(x[1] - 2 + 8*x[2] - x[2]^2)^2
  yb <- (3 - 4*x[2])^2 + 16*sqrt(x[3] + 1)*(2*x[3] - 1)^2
  return(ya + yb)
}

quackquack_detpep_curve <- function(){
  out <- list(input_dim=3)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  out$stochastic <- "n"


    RR <- cbind(c(0, 0, 0),
                c(1, 1, 1))
  rownames(RR) <- c("x1", "x2", "x3")
  out$input_range <- RR
  return(out)
}

#' The Dette & Pep 8-Dim Function
#'
#' Dimensions 8.
#' @param x Inputs of dimension (at least) 8. See below for details.
#' @param scale01 (No effect) When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @return Response.
#' @details Function is highly curved in first 3 dimensions, less so for inputs 4-8.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Dette, Holger, and Andrey Pepelyshev. "Generalized Latin hypercube design for computer experiments." Technometrics 52.4 (2010): 421-429.
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(3*n), nrow=n)
#' y <- apply(x, 1, detpep_curve, scale01=TRUE)
detpep8 <- function(x, scale01=TRUE){
  ya <- 4*(x[1] - 2 + 8*x[2] - x[2]^2)^2
  yb <- (3 - 4*x[2])^2 + 16*sqrt(x[3] + 1)*(2*x[3] - 1)^2
  yc <- 0
  for(j in 4:8){
    yc <- yc + j*log(1 + sum(x[3:j]))
  }
  return(ya + yb + yc)
}

quackquack_detpep8 <- function(){
  out <- list(input_dim=8)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  out$stochastic <- "n"


  RR <- cbind(rep(0, 8), rep(1, 8))
  rownames(RR) <- unlist(lapply(1:8,
                        function(zz) paste0("x", zz)))
  out$input_range <- RR
  return(out)
}


#' The Welch Screening Function
#'
#' Dimensions 20.
#' @param x Inputs of dimension (at least) 20. See below for details.
#' @param scale01 (No effect) When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @return Response.
#' @details A challenging high-dimensional problem used for variable screening purposes
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Welch, W. J., Buck, R. J., Sacks, J., Wynn, H. P., Mitchell, T. J., & Morris, M. D. (1992). Screening, predicting, and computer experiments. Technometrics, 34(1), 15-25.
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(20*n), nrow=n)
#' y <- apply(x, 1, welch20, scale01=TRUE)
welch20 <- function(x, scale01=TRUE){
  if(scale01){
    x <- x - 0.5
  }
  xx <- x
  x1  <- xx[1]
  x2  <- xx[2]
  x3  <- xx[3]
  x4  <- xx[4]
  x5  <- xx[5]
  x6  <- xx[6]
  x7  <- xx[7]
  x8  <- xx[8]
  x9  <- xx[9]
  x10 <- xx[10]
  x11 <- xx[11]
  x12 <- xx[12]
  x13 <- xx[13]
  x14 <- xx[14]
  x15 <- xx[15]
  x16 <- xx[16]
  x17 <- xx[17]
  x18 <- xx[18]
  x19 <- xx[19]
  x20 <- xx[20]

  term1 <- 5*x12 / (1+x1)
  term2 <- 5 * (x4-x20)^2
  term3 <- x5 + 40*x19^3 - 5*x19
  term4 <- 0.05*x2 + 0.08*x3 - 0.03*x6
  term5 <- 0.03*x7 - 0.09*x9 - 0.01*x10
  term6 <- -0.07*x11 + 0.25*x13^2 - 0.04*x14
  term7 <- 0.06*x15 - 0.01*x17 - 0.03*x18

  y <- term1 + term2 + term3 + term4 + term5 + term6 + term7
  return(y)
}

quackquack_welch20 <- function(){
  out <- list(input_dim=20)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  out$stochastic <- "n"


  RR <- cbind(rep(-0.5, 20),
              rep(0.5, 20))
  rownames(RR) <- unlist(lapply(1:20,
                                function(zz) paste0("x", zz)))
  out$input_range <- RR
  return(out)
}






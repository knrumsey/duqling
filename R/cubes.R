#' @name cubes
#' @rdname cubes
#'
#' @title Discontinuous Cube Functions
#'
#' @param x Inputs of dimension (at least) 8. See below for details.
#' @param scale01 (No effect here) When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @details Three cube functions
#' \enumerate{
#'    \item {cube3: a 3 dimensional test function equal to the sum of 3 indicator functions: f(x) = 1(0.2 < x1 < 0.5)1(0.1 < x2 < 0.6)1(x3 < 0.4) + 1(0.3 < x1 < 0.85)1(0.5 < x2 < 0.9)1(0.6 < x3) + 1(0.35 < x1 < 0.45)1(x2 < 0.75)}
#'    \item {cube3_rotate: equal to cube3(z1,z2,z3) after applying the rotation: 3z1 = x1 + x2 + x3, 3z2 = 1 + 2x1 - x2 + x3/10 , z3 = x3}
#'    \item {cube5: a 5 dimensional test function equal to cube3(x1,x2,x3) + prod(i=1:5)(0.25 < xi < 0.75) + 5*1(0.33 < x5)}
#' }
#'
#' @examples
#' n <- 100 #Number of observations
#' p <- 3   #Number of variables
#' X <- matrix(runif(n*p), nrow=n)
#' y <- apply(X, 1, cube3)
#' @export
cube3 <- function(x, scale01=TRUE){
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]

  c1 <- (0.2 < x1)*(x1 < 0.5)*(0.1 < x2)*(x2 < 0.6)*(x3 < 0.4)
  c2 <- (0.3 < x1)*(x1 < 0.85)*(0.5 < x2)*(x2 < 0.9)*(x3 < 0.8)
  c3 <- (0.35 < x1)*(x1 < 0.45)*(x2 < 0.75)

  c1 + c2 + c3
}

#' @name cube3_rotate
#' @rdname cube3_rotate
#'
#' @param x Inputs of dimension (at least) 8. See below for details.
#' @param scale01 (No effect here) When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#'
#' @title Discontinuous Cube Functions
#'
#' @export
cube3_rotate <- function(x, scale01=TRUE){
  z1 <- (x[1] + x[2] + x[3])/3
  z2 <- (1 + 2*x[1] - x[2] + x[3]/10)/3
  z3 <- x[3]
  cube3(c(z1,z2,z3))
}

#' @name cubes5
#' @rdname cubes5
#'
#' @param x Inputs of dimension (at least) 8. See below for details.
#' @param scale01 (No effect here) When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#'
#' @title Discontinuous Cube Functions
#'
#' @export
cube5 <- function(x, scale01=TRUE){
  cube3(x[1:3]) + prod((x[1:5] > 0.25)*(x[1:5] < 0.75)) + 5*(x[5] > 1/3)
}


quackquack_cube3 <- function(){
  out <- list(input_dim=3)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(0, 0, 0),
              c(1, 1, 1))
  rownames(RR) <- c("x1", "x2", "x3")
  out$input_range <- RR

  return(out)
}
quackquack_cube3_rotate <- quackquack_cube3()

quackquack_cube5 <- function(){
  out <- list(input_dim=5)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(0, 0, 0, 0, 0),
              c(1, 1, 1, 1, 1))
  rownames(RR) <- c("x1", "x2", "x3", "x4", "x5")
  out$input_range <- RR

  return(out)
}

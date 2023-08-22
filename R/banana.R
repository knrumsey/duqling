#' @title Rosenbrock's Banana Function
#'
#' @param x Inputs of dimension (at least) 2. See below for details.
#' @param scale01  (No effect) When TRUE, inputs are expected to be given on unit interval.
#' @param ab Hyperparameters to Rosenbrocks banana
#' @details A non-convec function introduced by Howard H. Rosenbrock in 1960. It is a difficult optimization test-problem, with a unique minimum at (1,1).
#' @references
#' Rosenbrock, HoHo. "An automatic method for finding the greatest or least value of a function." The computer journal 3.3 (1960): 175-184.
#' @rdname banana
#' @examples
#' n1 <- 100 #Number of observations in x1
#' n2 <- 70  #Number of observations in x2
#' x1grid <- seq(0, 1, length.out=100)
#' x2grid <- seq(0, 1, length.out=70)
#' X <- expand.grid(x1grid, x2grid)
#' y <- apply(X, 1, banana, scale01=TRUE)
#' image(matrix(y, nrow=length(X)), zlim=c(-1,3.5))
#' @export
banana <- function(x, scale01=FALSE, ab=c(1, 100)){
  if(scale01){
    RR <- cbind(c(-2,-1), c(2, 3))
    x[1:2] <- x[1:2]*(RR[,2] - RR[,1]) + RR[,1]
  }
  a <- ab[1]
  b <- ab[2]
  z <- (a-x[1])^2 + b*(x[2]-x[1]^2)^2
  return(z)
}

quackquack_banana <- function(){
  out <- list(input_dim=2)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  out$stochastic <- "n"

  RR <- cbind(c(-2,-1), c(2, 3))
  rownames(RR) <- c("x1", "x2")
  out$input_range <- RR
  return(out)
}


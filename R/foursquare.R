#' The Foursquare Function
#'
#' Dimensions: 2
#'
#' @param x Inputs of dimension (at least) 7. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @return The response C is cycle time (the time it takes to complete one cycle), in seconds
#' @details Each of the four quadrants can be modeled effectively with a different approach (rbf, linear, mars, trees).
#' The function response is $y = f1 + f2 + f3 + f4$ where
#' \describe{
#'   \item{f1}{An RBF centered at (0.75, 0.75) with variance of 0.13 and a correlation of -0.5}
#'   \item{f2}{The linear plane x_2 - 1.375*x_1}
#'   \item{f3}{A mars basis function with a knot at (0.35, 0.35)}
#'   \item{f4}{A scaled/shifted version of \code{duqling::sharkfin}, designed for tree based methods.}
#' }
#' @export
#' @examples
#' n <- 10
#' p <- 2
#' x <- matrix(runif(p*n), nrow=n)
#' y <- apply(x, 1, foursquare, scale01=TRUE)
foursquare <- function(x, scale01=TRUE, ftype="all"){
  ind1 <- x[1] > 0.5
  ind2 <- x[2] > 0.5

  # func 1
  mu <- matrix((x - c(0.75, 0.75)), ncol=2)
  #Sigma <- matrix(c(1, -0.5, -0.5, 1), nrow=2, byrow=TRUE)
  Sigma <- matrix(c(1, 0, 0, 1), nrow=2, byrow=TRUE)
  f1 <- exp(- 30*mu%*%solve(Sigma)%*%t(mu))

  # func 2
  f2 <- x[2] - 1.375*x[1]

  # func 3
  pos <- function(xx) (abs(xx) + xx)/2
  a <- pos(-(x[1] - 0.35))
  b <- pos(-(x[2] - 0.35))
  f3 <- 8.16*a*b

  # func 4
  #z <- (x[1:2] - c(0.5, 0))*2
  #sh <- sharkfin(z, steps=2)
  #f4 <- (sh + 1)/2
  fa <- 0.33*(x[1] > 0.75)
  fb <- 0.27*(x[1] > 0.75)*(x[2] > 0.25)
  fc <- 0.09*(x[1] > 0.75)*(x[2] > 0.25)
  fd <- 0.10*(x[1] > 0.85)*(x[2] > 0.25)
  fe <- 0.45*(x[1] > 0.50)*(x[2] < 0.50)
  f4 <- (fa + fb + fc + fd + fe)

  # Put functions togethers
  #y <- ind1*ind2*f1 + (1-ind1)*ind2*f2 + (1-ind1)*(1-ind2)*f3 + ind1*(1-ind2)*f4

  if(ftype == "all"){
    y <- f1 + f2 + f3 + f4
  }else{
    y <- get(paste0("f", ftype))
  }
  return(y)
}


quackquack_foursquare <- function(){
  out <- list(input_dim=2)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(0, 0), c(1, 1))
  rownames(RR) <- c("x1", "x2")
  out$input_range <- RR
  return(out)
}












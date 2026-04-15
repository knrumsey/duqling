#' The Gamma Mix Function
#'
#' Dimensions: 7. Returns the value of a two-component gamma mixture density at a given location, with user-defined parameters and an optional horizontal shift.
#'
#' @param x Inputs of dimension (at least) 7. See below for details.
#' @param scale01 When TRUE, inputs are expected to be on the unit interval and are internally adjusted to their native range.
#' @return Value of the gamma mixture density at the location x[1].
#' @details
#' The function is defined as:
#' \deqn{f(x) = w \cdot \text{dgamma}(x_1 - m; a_1, b_1) + (1-w) \cdot \text{dgamma}(x_1 - m; a_2, b_2)}
#' where
#' \itemize{
#'   \item \code{x[1]}: location to evaluate (default range [0, 50])
#'   \item \code{x[2]}: shape parameter \code{a1} for first gamma ([1, 10])
#'   \item \code{x[3]}: rate parameter \code{b1} for first gamma ([0.5, 5])
#'   \item \code{x[4]}: shape parameter \code{a2} for second gamma ([1, 10])
#'   \item \code{x[5]}: rate parameter \code{b2} for second gamma ([0.5, 5])
#'   \item \code{x[6]}: mixing weight \code{w} ([0, 1])
#'   \item \code{x[7]}: horizontal shift \code{m} ([0, 10])
#' }
#' The density is evaluated at \code{x[1]} minus the shift \code{m}. If the argument to dgamma is negative, the density is zero.
#' @export
#' @examples
#' n <- 4
#' X <- matrix(runif(n*7), nrow=n)
#' y <- apply(X, 1, gamma_mix, scale01=TRUE)
gamma_mix <- function(x, scale01=TRUE){
  # Ranges
  RR <- cbind(c(0,    1,   0.5,  1,   0.5, 0, 0),    # min
              c(50,  10,   5,   10,    5,  1, 10))    # max
  if(scale01){
    x[1:7] <- x[1:7]*(RR[,2] - RR[,1]) + RR[,1]
  }
  loc <- x[1]
  a1 <- x[2]
  b1 <- x[3]
  a2 <- x[4]
  b2 <- x[5]
  w  <- x[6]
  m  <- x[7]
  z <- loc - m
  d1 <- dgamma(z, shape=a1, rate=b1)
  d2 <- dgamma(z, shape=a2, rate=b2)
  y <- (w * d1 + (1-w) * d2) * (z >= 0)
  return(y)
}

#' @export
quackquack_gamma_mix <- function(){
  out <- list(input_dim=7)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  RR <- cbind(c(0,    1,   0.5,  1,   0.5, 0, 0),    # min
              c(50,  10,   5,   10,    5,  1, 10))   # max
  rownames(RR) <- c("x", "a1", "b1", "a2", "b2", "w", "m")
  out$input_range <- RR
  return(out)
}

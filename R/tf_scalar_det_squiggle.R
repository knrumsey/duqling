#' @title The Squiggle Function
#'
#' @param x Inputs of dimension (at least) 2. See below for details.
#' @param scale01  (No effect) When TRUE, inputs are expected to be given on unit interval.
#' @param sigma A scaling parameter
#' @details A bivariate squiggle function which has regions of low activity as well as low activity regions.
#' When noise != 0, the function is stochastic with a heteroskedastic error function.
#'
#' @references
#' Add reference here, if we ever publish with this function.
#' @rdname squiggle
#' @examples
#' fname <- "squiggle"
#' n <- 10
#' p <- quack(fname, verbose=FALSE)$input_dim
#' x <- matrix(runif(n*p), nrow=n)
#' y <- eval_duq(fname, x)
#'
#' show_function_2d("squiggle")
#' @export
squiggle <- function(x, scale01=TRUE, sigma = 0.05) {
  stats::dnorm(x[2], mean = sin(2 * pi * x[1] ^ 2) / 4 - x[1] / 10 + .5, sd = sigma)*x[1]*x[2]
}

quackquack_squiggle <- function(){
  out <- list(input_dim=2)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  out$stochastic <- "n"

  RR <- cbind(c(0,0), c(1,1))
  rownames(RR) <- c("x1", "x2")
  out$input_range <- RR
  return(out)
}


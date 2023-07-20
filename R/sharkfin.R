#' Sharkfin Function
#'
#' Dimensions default is 3, but can be any dimension. For inert variables, \code{active_dim} must be specified.
#'
#' @param x Inputs of dimension (at least) 8. See below for details.
#' @param scale01  When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param steps Default is 3. For larger values of \code{steps}, the function becomes more fractal-like and takes longer to run.
#' @param active_dim Only the first \code{min(active_dim, length(x))} variables will be used. All other variables are inert.
#' @return Univariate response representing small scale structure
#' @details A non-smooth test function designed with trees in mind. As steps -> Inf, this function is nowhere-differentiable.
#' @references
#' Collins, Gavin, Devin Francom, and Kellin Rumsey. "Bayesian projection pursuit regression." arXiv preprint arXiv:2210.09181 (2022).
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(3*n), nrow=n)
#' y <- apply(x, 1, sharkfin)
sharkfin <- function(x, scale01=TRUE, steps=3, active_dim=length(x)){
  k <- steps
  p <- min(active_dim, length(x))
  fib <- c(0, 1)
  grid.list <- vector('list', k)
  for(j in 1:k){
    fib[j + 2] <- sum(fib[j + c(0, 1)])
    grid.list[[j]] <- fib[j + 2] * c(0, 1, -1)
  }
  grid <- expand.grid(grid.list)
  f <- apply(grid, 1, sum)
  f_max <- max(f)
  cutpoints <- seq(0, 1, length.out = length(f) + 1)

  F_x <- matrix(0, ncol=length(x), nrow=1)

  for(j in 1:p){
    for(cut in 1:length(f)){
      flag <- x[j] > cutpoints[cut]  &  x[j] <= cutpoints[cut + 1]
      F_x[flag, j] <- f[cut] / f_max
    }
  }
  y <- apply(F_x, 1, mean)
  return(y)
}


quackquack_sharkfin <- function(){
  out <- list(input_dim=3)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(0, 0, 0), c(1, 1, 1))
  rownames(RR) <- c("x1", "x2", "x3")
  out$input_range <- RR
  return(out)
}

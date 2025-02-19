#' @title The 100D Function
#'
#' @description Dimension: 100
#'
#' @param x Inputs of dimension (at least) 2. See below for details.
#' @param scale01  (No effect) When TRUE, inputs are expected to be given on unit interval.
#' @param M The number of active variables (default M=100, (54 <= M <= 100))
#' @details The 100-dimensional function is a high-dimensional function derived from a more generic example detailed below. This function was designed for sensitivity analysis: the first-order sensitivity indices of the input variables generally nonlinearly increase with their index, with certain variables having especially high sensitivity (Lüthen et al, 2021).
#' @references
#' Lüthen, Nora, Stefano Marelli, and Bruno Sudret. "Sparse polynomial chaos expansions: Literature survey and benchmark." SIAM/ASA Journal on Uncertainty Quantification 9.2 (2021): 593-649.
#'
#' UQLab - The Framework for Uncertainty Quantification. “Sensitivity: Sobol’ indices of a high-dimensional function.” Retrieved July 3, 2024, from https://www.uqlab.com/sensitivity-high-dimension.
#' @rdname onehundred
#' @examples
#' n1 <- 100 #Number of observations in x1
#' n2 <- 70  #Number of observations in x2
#' x1grid <- seq(0, 1, length.out=100)
#' x2grid <- seq(0, 1, length.out=70)
#' X <- expand.grid(x1grid, x2grid)
#' y <- apply(X, 1, banana, scale01=TRUE)
#' image(matrix(y, nrow=length(X)), zlim=c(-1,3.5))
#' @export
onehundred <- function(x, scale01=TRUE, M=100){
  if(scale01){
    lb <- rep(1, 100)
    ub <- rep(2, 100)
    ub[20] <- 3
    RR <- cbind(lb, ub)
    x[1:100] <- x[1:100]*(RR[,2] - RR[,1]) + RR[,1]
  }

  linear <- seq_along(x)
  select <- as.numeric(linear <= M)

  term1 <- 3 - 5/M*sum(linear * select * x)
  term2 <- 1/M*sum(linear * select * x^3)
  term3 <- 1/(3*M) * sum(linear * select * log(x^2 + x^4))
  term4 <- x[1]*x[2]^2 + x[2]*x[4] - x[3]*x[5] + x[51] + x[50]*x[54]^2
  res <- term1 + term2 + term3 + term4
  return(res)
}


onehundred_quackquack <- function(){
  out <- list(input_dim=100)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  out$stochastic <- "n"

  lb <- rep(1, 100)
  ub <- rep(2, 100)
  ub[20] <- 3
  RR <- cbind(lb, ub)
  rownames(RR) <- unlist(lapply(1:100,
                                function(zz) paste0("x", zz)))
  out$input_range <- RR
  return(out)
}

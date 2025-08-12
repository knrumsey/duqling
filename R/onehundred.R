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


quackquack_onehundred <- function(){
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



#' @title The Gradient of the 100D Function
#'
#' @description Input Dimension: 100, Response Dimension: 100
#'
#' @param x Inputs of dimension (at least) 2. See below for details.
#' @param scale01  (No effect) When TRUE, inputs are expected to be given on unit interval.
#' @param M The number of active variables (default M=100, (54 <= M <= 100))
#' @details The 100-dimensional function is a high-dimensional function derived from a more generic example detailed below. This function was designed for sensitivity analysis: the first-order sensitivity indices of the input variables generally nonlinearly increase with their index, with certain variables having especially high sensitivity (Lüthen et al, 2021).
#' @references
#' Lüthen, Nora, Stefano Marelli, and Bruno Sudret. "Sparse polynomial chaos expansions: Literature survey and benchmark." SIAM/ASA Journal on Uncertainty Quantification 9.2 (2021): 593-649.
#'
#' UQLab - The Framework for Uncertainty Quantification. “Sensitivity: Sobol’ indices of a high-dimensional function.” Retrieved July 3, 2024, from https://www.uqlab.com/sensitivity-high-dimension.
#' @rdname d_onehundred
#' @examples
#' n <- 1000
#' p <- 100
#' X <- matrix(runif(n*p), nrow=n, ncol=p)
#' y <- apply(X, 1, d_onehundred, scale01=TRUE)
#' # not run
#' # fit <- basssPCA(X, t(y))
#' @export
d_onehundred <- function(x, scale01=TRUE, M=100){
  if(M < 55){
    warning("M must be at least 55")
    M <- 55
  }
  if(scale01){
    lb <- rep(1, 100)
    ub <- rep(2, 100)
    ub[20] <- 3
    RR <- cbind(lb, ub)
    x[1:100] <- x[1:100]*(RR[,2] - RR[,1]) + RR[,1]
  }

  res <- rep(NA, M)
  # Linear gradient terms
  for(k in seq_len(M)){
    res[k] <- -5 * k / M +
              3 * k * x[k]^2 / M +
              k * (4 * x[k]^2 + 2) / (x[k]^3 + x[k]) / (3 * M)
  }

  # Interaction terms
  res[1]  <- res[1] + x[2]^2
  res[2]  <- res[2] + 2 * x[1] * x[2] + x[4]
  res[3]  <- res[3] - x[5]
  res[4]  <- res[4] + x[2]
  res[5]  <- res[5] - x[3]
  res[50] <- res[50] + x[54]^2
  res[51] <- res[51] + 1
  res[54] <- res[54] + 2 * x[54] * x[50]

  # Chain rule
  if (scale01) {
    res <- res * (ub[seq_len(M)] - lb[seq_len(M)])
  }

  return(res)
}

quackquack_d_onehundred <- function(){
  out <- list(input_dim=100, output_dim=100)
  out$input_cat <- FALSE
  out$response_type <- "multi"
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



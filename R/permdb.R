#' The Permdb Function
#'
#' Dimensions: Variable (default 16). The permdb function is a well-known deterministic test function used in high-dimensional optimization and uncertainty quantification. It is challenging due to its intricate global structure and numerous local minima.
#'
#' @param x Inputs of dimension (at least) \code{d}. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range.
#' @param d Dimension of the input (default 16).
#' @param beta Beta parameter in the perm function (default 0.5).
#' @return Scalar output.
#' @details
#' The permdb function is defined as:
#' \deqn{f(x) = \sum_{j=1}^d \left[ \sum_{i=1}^d (i^j + \beta) \left( \left(\frac{x_i}{i}\right)^j - 1 \right) \right]^2}
#' with the typical input domain \eqn{x_i \in [-d, d]}.
#' @references
#' Dixon, L. C. W., & Price, R. (1981). The perm function. In Towards Global Optimization 2 (pp. 169–204).
#' @export
#' @examples
#' n <- 3
#' x <- matrix(runif(n*16), nrow=n)
#' y <- apply(x, 1, permdb, scale01=TRUE)
permdb <- function(x, scale01=TRUE, d=16, beta=0.5){
  if(scale01){
    RR <- cbind(rep(-d, d), rep(d, d))
    x[1:d] <- x[1:d]*(RR[,2] - RR[,1]) + RR[,1]
  }
  ii <- c(1:d)
  jj <- matrix(rep(ii, times=d), d, d, byrow=TRUE)
  xxmat <- matrix(rep(x[1:d], times=d), d, d, byrow=TRUE)
  inner <- rowSums((jj^ii + beta) * ((xxmat / jj)^ii - 1))
  outer <- sum(inner^2)
  y <- outer
  return(y)
}

#' @export
quackquack_permdb <- function(){
  out <- list(input_dim = 16)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  RR <- cbind(rep(-16, 16), rep(16, 16))
  rownames(RR) <- paste0("x", 1:16)
  out$input_range <- RR
  return(out)
}

#' @name simple_poly
#' @rdname simple_poly
#'
#' @title Simple Polynomial Function from Rumsey et al 2023
#' @description
#' Dimensions: 2.
#'
#' @param x Inputs of dimension (at least) 2. See below for details.
#' @param scale01 (Ignored here) When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param beta default 1/9
#' @return A scalar response
#' @details a simple polynomial function.
#' @references
#' Rumsey, Kellin N., et al. "Co-Active Subspace Methods for the Joint Analysis of Adjacent Computer Models." arXiv preprint arXiv:2311.18146 (2023).
NULL

#' @rdname simple_poly
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, simple_poly, scale01=TRUE)
simple_poly <- function(x, beta=1/9, scale01=TRUE){
  x[1]^2 + x[1]*x[2] + beta*x[2]^3
}

quackquack_simple_poly <- function(){
  out <- list(input_dim=2)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(rep(0, 2),
              rep(1, 2))
  rownames(RR) <- c("x1", "x2")
  out$input_range <- RR
  return(out)
}

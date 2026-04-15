#' Mock Ingition Function
#'
#' Dimensions: 10 The complex mock ignition function from Section E of Hatfield et al. 2019.
#'
#' @param x Inputs of dimension (at least) 10. See below for details.
#' @param scale01 (No effect here). When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range.
#' @return The response is mock yield of an ignition cliff.
#' @details From Hatfield et al. 2019, the mock-yield is r^5(1 + 100000 + (1+erf(10(r-2)))) where r is the radius from the origin in this 10-D space.
#' @references
#' Hatfield, P., Rose, S., Scott, R., Almosallam, I., Roberts, S., & Jarvis, M. (2019). Using sparse Gaussian processes for predicting robust inertial confinement fusion implosion yields. IEEE Transactions on Plasma Science, 48(1), 14-21.
#'
#' @export
#' @examples
#' p <- 10
#' n <- 20
#' x <- matrix(runif(n*p), nrow=n)
#' y <- apply(x, 1, ignition, scale01=TRUE)
ignition<-function(x, scale01=TRUE){
  r<-sqrt(sum(x[1:10]^2))
  return(log10(r^5*(1+100000*(2*stats::pnorm(sqrt(2)*10*(r-2))))))
}


quackquack_ignition <- function(){
  out <- list(input_dim=10)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(rep(0, 10),
              rep(1, 10))
  rownames(RR) <- paste0("x", 1:10)
  out$input_range <- RR
  return(out)
}

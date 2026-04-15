#' The Ebola Function
#'
#' Dimensions 8. Calculates the estimated basic reproduction number, R0, for an Ebola outbreak.
#'
#' @param x Inputs of dimension (at least) 8. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range.
#' @param location Default value is "liberia" but "sierra_leone" is also accepted.
#' @return Basic reproduction number
#' @details See section 5.1 of reference for details and further references.
#' @references Duan, Hui, and Giray Okten. "Derivative-based Shapley value for global sensitivity analysis and machine learning explainability." International Journal for Uncertainty Quantification 15.1 (2025).
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, ebola)
ebola <- function(x, scale01=TRUE, location="liberia"){
  if(scale01){
    RR <- cbind(c(0.1, 0.1, 0.05, 0.41, 0.0276, 0.081, 0.25, 0.0833),
                c(0.4, 0.4, 0.2, 1, 0.1702, 0.21, 0.5, 0.7))
    x[1:8] <- x[1:8]*(RR[,2] - RR[,1]) + RR[,1]
  }
  if(location == "sierra_leone"){
    x[5] <- (x[5] - 0.0276) / (0.1702 - 0.0276)
    x[5] <- 0.0275 + x[5] * (0.1569 - 0.0275)

    x[6] <- (x[6] - 0.081) / (0.21 - 0.081)
    x[6] <- 0.081 + x[6] *(0.384 - 0.1236)
  }

  beta1  <- x[1]
  beta2  <- x[2]
  beta3  <- x[3]
  rho    <- x[4]
  gamma1 <- x[5]
  gamma2 <- x[6]
  omega  <- x[7]
  psi    <- x[8]

  R0 <- (beta1 + beta2*rho*gamma1/omega + beta3*psi/gamma2) / (gamma1 + psi)
  return(R0)
}

quackquack_ebola <- function(){
  out <- list(input_dim=8)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(0.1, 0.1, 0.05, 0.41, 0.0276, 0.081, 0.25, 0.0833),
              c(0.4, 0.4, 0.2, 1, 0.1702, 0.21, 0.5, 0.7))
  rownames(RR) <- c("beta1", "beta2", "beta3", "rho", "gamma1", "gamma2", "omega", "psi")
  out$input_range <- RR
  return(out)
}

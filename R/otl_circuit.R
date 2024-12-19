#' OTL Circuit Function
#'
#' Dimensions: 6 The OTL Circuit function models an output transformerless push-pull circuit.
#'
#' @param x Inputs of dimension (at least) 7. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @return The response Vm is midpoint voltage
#' @details For details on this function, see \href{https://www.sfu.ca/~ssurjano/otlcircuit.html}{the VLSE}.
#' Parameter ranges:
#' \describe{
#'   \item{x1 = Rb1}{resistance b1 (K-Ohms), [50, 150]}
#'   \item{x2 = Rb2}{resistance b2 (K-Ohms), [25, 70]}
#'   \item{x3 = Rf}{resistance f (K-Ohms),   [0.5, 3]}
#'   \item{x4 = Rc1}{resistance c1 (K-Ohms), [1.2, 2.5]}
#'   \item{x5 = Rc2}{resistance c2 (K-Ohms), [0.25, 1.2]}
#'   \item{x5 = beta}{current gain (Amps), [50, 300]}
#' }
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Ben-Ari, E. N., & Steinberg, D. M. (2007). Modeling data from computer experiments: an empirical comparison of kriging with MARS and projection pursuit regression. Quality Engineering, 19(4), 327-338.
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(6*n), nrow=n)
#' y <- apply(x, 1, circuit, scale01=TRUE)
circuit <- function(x, scale01=TRUE){
  if(scale01){
    RR <- cbind(c(50, 25, 0.5, 1.2, 0.25, 50),
                c(150, 70, 3, 2.5, 1.2, 300))
    x[1:6] <- x[1:6]*(RR[,2] - RR[,1]) + RR[,1]
  }

  Rb1  <- x[1]
  Rb2  <- x[2]
  Rf   <- x[3]
  Rc1  <- x[4]
  Rc2  <- x[5]
  beta <- x[6]

  Vb1 <- 12*Rb2 / (Rb1+Rb2)
  term1a <- (Vb1+0.74) * beta * (Rc2+9)
  term1b <- beta*(Rc2+9) + Rf
  term1 <- term1a / term1b

  term2a <- 11.35 * Rf
  term2b <- beta*(Rc2+9) + Rf
  term2 <- term2a / term2b

  term3a <- 0.74 * Rf * beta * (Rc2+9)
  term3b <- (beta*(Rc2+9)+Rf) * Rc1
  term3 <- term3a / term3b

  Vm <- term1 + term2 + term3
  return(Vm)
}


quackquack_circuit <- function(){
  out <- list(input_dim=6)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(50, 25, 0.5, 1.2, 0.25, 50),
              c(150, 70, 3, 2.5, 1.2, 300))
  rownames(RR) <- c("Rb1", "Rb2", "Rf", "Rc1", "Rc2", "beta")
  out$input_range <- RR
  return(out)
}

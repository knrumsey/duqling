#' Piston Simulation Function
#'
#' Dimensions: 7 The Piston Simulation function models the circular motion of a piston within a cylinder. It involves a chain of nonlinear functions.
#'
#' @param x Inputs of dimension (at least) 7. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @return The response C is cycle time (the time it takes to complete one cycle), in seconds
#' @details For details on the piston function, see \href{https://www.sfu.ca/~ssurjano/piston.html}{the VLSE}.
#' Parameter ranges:
#' \describe{
#'   \item{x1 = M}{piston weight (kg), [30, 60]}
#'   \item{x2 = S}{piston surface area (m^2), [0.005, 0.020]}
#'   \item{x3 = V0}{initial gas volume (m^3), [0.002, 0.010]}
#'   \item{x4 = k}{spring coefficient (N/m), [1000, 5000]}
#'   \item{x5 = P0}{atmospheric pressure (N/m^2), [90000, 110000]}
#'   \item{x6 = Ta}{ambient temperature (K), [290, 296]}
#'   \item{x7 = T0}{filling gas temperature (K), [340, 360]}
#' }
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Kenett, R., & Zacks, S. (1998). Modern industrial statistics: design and control of quality and reliability. Pacific Grove, CA: Duxbury press.
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, piston, scale01=TRUE)
piston <- function(x, scale01=FALSE){
  if(scale01){
    RR <- cbind(c(30, 0.005, 0.002, 1000, 90000, 290, 340),
                c(60, 0.020, 0.010, 5000, 110000, 296, 360))
    x[1:7] <- x[1:7]*(RR[,2] - RR[,1]) + RR[,1]
  }

  M  <- x[1]
  S  <- x[2]
  V0 <- x[3]
  k  <- x[4]
  P0 <- x[5]
  Ta <- x[6]
  T0 <- x[7]

  Aterm1 <- P0 * S
  Aterm2 <- 19.62 * M
  Aterm3 <- -k*V0 / S
  A <- Aterm1 + Aterm2 + Aterm3

  Vfact1 <- S / (2*k)
  Vfact2 <- sqrt(A^2 + 4*k*(P0*V0/T0)*Ta)
  V <- Vfact1 * (Vfact2 - A)

  fact1 <- M
  fact2 <- k + (S^2)*(P0*V0/T0)*(Ta/(V^2))

  C <- 2 * pi * sqrt(fact1/fact2)
  return(C)
}


#' Stochastic Piston Simulation Function
#'
#' Dimensions: 5 The Piston Simulation function models the circular motion of a piston within a cylinder. It involves a chain of nonlinear functions.
#'
#' @param x Inputs of dimension (at least) 5. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param Ta_generate The generating distribution for the ambient temperature (on (0,1) scale). Default is `rbeta(1, 10, 1.5)`
#' @param P0_generate The generating distribution for the atmospheric pressure (on (0,1) scale). Default is `runif(1, 0.49, 0.51)`
#' @return The response C is cycle time (the time it takes to complete one cycle), in seconds
#' @details For details on the piston function, see \href{https://www.sfu.ca/~ssurjano/piston.html}{the VLSE}.
#' Parameter ranges:
#' \describe{
#'   \item{x1 = M}{piston weight (kg), [30, 60]}
#'   \item{x2 = S}{piston surface area (m^2), [0.020, 0.045]}
#'   \item{x3 = V0}{initial gas volume (m^3), [0.002, 0.010]}
#'   \item{x4 = k}{spring coefficient (N/m), [1000, 5000]}
#'   \item{x5 = T0}{filling gas temperature (K), [340, 360]}
#' }
#' This is a stochastic version of the piston function. Note that the range of the piston surface area is different from the canonical piston function. The distribution of the response is heavily influenced by the stochastic process governing ambient temperature. The response distribution is less homoskedastic for larger ranges of atmospheric pressure.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Kenett, R., & Zacks, S. (1998). Modern industrial statistics: design and control of quality and reliability. Pacific Grove, CA: Duxbury press.
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, piston, scale01=TRUE)
stochastic_piston <- function(x, scale01=TRUE,
                              Ta_generate=function() rbeta(1, 10, 15), P0_generate=function() runif(1, 0.49, 0.51)){
  Ta <- Ta_generate()
  P0 <- P0_generate()
  x <- c(x[1:4], P0, Ta, x[5])
  if(scale01){
    if(scale01){
      RR <- cbind(c(30, 0.005, 0.002, 1000, 90000, 290, 340),
                  c(60, 0.020, 0.010, 5000, 110000, 296, 360))
      x[1:7] <- x[1:7]*(RR[,2] - RR[,1]) + RR[,1]
    }
  }

  M  <- x[1]
  S  <- x[2]
  V0 <- x[3]
  k  <- x[4]
  P0 <- x[5]
  Ta <- x[6]
  T0 <- x[7]

  Aterm1 <- P0 * S
  Aterm2 <- 19.62 * M
  Aterm3 <- -k*V0 / S
  A <- Aterm1 + Aterm2 + Aterm3

  Vfact1 <- S / (2*k)
  Vfact2 <- sqrt(A^2 + 4*k*(P0*V0/T0)*Ta)
  V <- Vfact1 * (Vfact2 - A)

  fact1 <- M
  fact2 <- k + (S^2)*(P0*V0/T0)*(Ta/(V^2))

  C <- 2 * pi * sqrt(fact1/fact2)
  return(C)
}



quackquack_piston <- function(){
  out <- list(input_dim=7)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(30, 0.005, 0.002, 1000, 90000, 290, 340),
              c(60, 0.020, 0.010, 5000, 110000, 296, 360))
  rownames(RR) <- c("M", "S", "V0", "k", "P0", "Ta", "T0")
  out$input_range <- RR
  return(out)
}

quackquack_stochastic_piston <- function(){
  out <- list(input_dim=5)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(30, 0.02, 0.002, 1000, 340),
              c(60, 0.045, 0.010, 5000, 360))
  rownames(RR) <- c("M", "S", "V0", "k", "T0")
  out$input_range <- RR
  return(out)
}

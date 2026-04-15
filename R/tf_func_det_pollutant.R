#' The Environmental Model for Pollutant Spill in a Channel
#'
#' Dimensions 4. Output dimension is a space-time field. The Environmental Model function models a pollutant spill caused by a chemical accident. C(x) is the concentration of the pollutant at the space-time vector (s, t), where 0 ≤ s ≤ 3 and t > 0. I is the indicator function.
#'
#' @param x Inputs of dimension (at least) 4. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param space a vector of locations along the channel (0 is location of the first spill).
#' @param time a vector of times. (0 is the time of the first spill)
#' @return C(x) is the concentration of the pollutant at the space-time vector (s, t)
#' @details For details on the pollutant function, see \href{https://www.sfu.ca/~ssurjano/environ.html}{the VLSE}.
#' Parameter ranges:
#' \describe{
#'   \item{x1 = M}{mass of pollutant spilled at each location, [7, 13]}
#'   \item{x2 = D}{diffusion rate in the channel, [0.02, 0.12]}
#'   \item{x3 = L}{location of the second spill, [0.01, 3]}
#'   \item{x4 = tau}{time of the second spill, [30.01, 30.295]}
#' }
#' For a univariate version of this function, see \code{\link[duqling]{pollutant_uni}}
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Bliznyuk, N., Ruppert, D., Shoemaker, C., Regis, R., Wild, S., & Mugunthan, P. (2008). Bayesian calibration and uncertainty analysis for computationally expensive models using optimization and radial basis function approximation. Journal of Computational and Graphical Statistics, 17(2).
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, pollutant, scale01=TRUE)
pollutant <- function(x, scale01=TRUE,
                            space=c(0.5, 1, 1.5, 2, 2.5),
                            time=seq(from=0.3, to=60, by=0.3)){
  if(scale01){
    RR <- cbind(c(7, 0.02, 0.01, 30.01),
                c(13, 0.12, 3, 30.295))
    x[1:4] <- x[1:4]*(RR[,2] - RR[,1]) + RR[,1]
  }


  M   <- x[1]
  D   <- x[2]
  L   <- x[3]
  tau <- x[4]
  s   <- space
  t   <- time


  ds <- length(s)
  dt <- length(t)
  dY <- ds * dt
  Y <- matrix(0, ds, dt)

  # Create matrix Y, where each row corresponds to si and each column
  # corresponds to tj.
  for (ii in 1:ds) {
    si <- s[ii]
    for (jj in 1:dt) {
      tj <- t[jj]

      term1a <- M / sqrt(4*pi*D*tj)
      term1b <- exp(-si^2 / (4*D*tj))
      term1 <- term1a * term1b

      term2 <- 0
      if (tau < tj) {
        term2a <- M / sqrt(4*pi*D*(tj-tau))
        term2b <- exp(-(si-L)^2 / (4*D*(tj-tau)))
        term2 <- term2a * term2b
      }

      C <- term1 + term2
      Y[ii, jj] <- sqrt(4*pi) * C
    }
  }

  # Convert the matrix into a vector (by rows).
  Yrow <- t(Y)
  y <- t(as.vector(Yrow))
  return(y)
}

quackquack_pollutant <- function(){
  out <- list(input_dim=4)
  out$input_cat <- FALSE
  out$response_type <- "func"


  RR <- cbind(c(7, 0.02, 0.01, 30.01),
              c(13, 0.12, 3, 30.295))
  rownames(RR) <- c("M", "D", "L", "tau")
  out$input_range <- RR
  return(out)
}

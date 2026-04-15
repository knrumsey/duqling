#' Robot Arm Function
#'
#' Dimensions: 8 Used commonly in neural network papers, models the position of a robot arm which has four segments. While the shoulder is fixed at the origin, the four segments each have length Li, and are positioned at an angle θi (with respect to the horizontal axis), for i = 1, …, 4
#'
#' @param x Inputs of dimension (at least) 8. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @return The response is the distance from the end of the robot arm to the origin, on the (u, v)-plane.
#' @details For details on this function, see \href{https://www.sfu.ca/~ssurjano/robot.html}{the VLSE}.
#' Parameter ranges:
#' \describe{
#'   \item{x1 = theta1}{angle of 1st arm segment, [0, 2*pi]}
#'   \item{x2 = theta2}{angle of 2nd arm segment, [0, 2*pi]}
#'   \item{x3 = theta3}{angle of 3rd arm segment, [0, 2*pi]}
#'   \item{x4 = theta4}{angle of 4th arm segment, [0, 2*pi]}
#'   \item{x5 = L1}{length of 1st arm segment,    [0, 1]}
#'   \item{x6 = L2}{length of 2nd arm segment,    [0, 1]}
#'   \item{x7 = L3}{length of 3rd arm segment,    [0, 1]}
#'   \item{x8 = L4}{length of 4th arm segment,    [0, 1]}
#' }
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Ben-Ari, E. N., & Steinberg, D. M. (2007). Modeling data from computer experiments: an empirical comparison of kriging with MARS and projection pursuit regression. Quality Engineering, 19(4), 327-338.
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, robot, scale01=TRUE)
robot <- function(x, scale01=TRUE){
  if(scale01){
    RR <- cbind(rep(0, 8),
                c(rep(2*pi, 4), rep(1, 4)))
    x[1:8] <- x[1:8]*(RR[,2] - RR[,1]) + RR[,1]
  }

  theta <- x[1:4]
  L     <- x[5:8]

  thetamat <- matrix(rep(theta,times=4), 4, 4, byrow=TRUE)
  thetamatlow <- thetamat
  thetamatlow[upper.tri(thetamatlow)] <- 0
  sumtheta <- rowSums(thetamatlow)

  u <- sum(L*cos(sumtheta))
  v <- sum(L*sin(sumtheta))

  y <- (u^2 + v^2)^(0.5)
  return(y)
}


quackquack_robot <- function(){
  out <- list(input_dim=8)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(rep(0, 8),
              c(rep(2*pi, 4), rep(1, 4)))
  rownames(RR) <- c("a1", "a2", "a3", "a4", "L1", "L2", "L3", "L4")
  out$input_range <- RR
  return(out)
}

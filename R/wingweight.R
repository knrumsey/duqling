#' Wing Weight Function
#'
#' Dimensions: 10 Models a light aircraft wing.
#'
#' @param x Inputs of dimension (at least) 10. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @return The response is the wing's wieght
#' @details For details on this function, see \href{https://www.sfu.ca/~ssurjano/robot.html}{the VLSE}.
#' For parameter ranges, see the \href{https://www.sfu.ca/~ssurjano/robot.html}{the VLSE}.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#'Forrester, A., Sobester, A., & Keane, A. (2008). Engineering design via surrogate modelling: a practical guide. Wiley.
#'
#' Moon, H. (2010). Design and Analysis of Computer Experiments for Screening Input Variables (Doctoral dissertation, Ohio State University).
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(10*n), nrow=n)
#' y <- apply(x, 1, wingweight, scale01=TRUE)
wingweight <- function(x, scale01=FALSE){
  if(scale01){
    RR <- cbind(c(150, 220, 6, -10, 16, 0.5, 0.08, 2.5, 1700, 0.025),
                c(200, 300, 10, 10, 45, 1.0, 0.18, 6.0, 2500, 0.08))
    x[1:10] <- x[1:10]*(RR[,2] - RR[,1]) + RR[,1]
  }

  Sw      <- x[1]
  Wfw     <- x[2]
  A       <- x[3]
  LamCaps <- x[4] * (pi/180)
  q       <- x[5]
  lam     <- x[6]
  tc      <- x[7]
  Nz      <- x[8]
  Wdg     <- x[9]
  Wp      <- x[10]

  fact1 <- 0.036 * Sw^0.758 * Wfw^0.0035
  fact2 <- (A / ((cos(LamCaps))^2))^0.6
  fact3 <- q^0.006 * lam^0.04
  fact4 <- (100*tc / cos(LamCaps))^(-0.3)
  fact5 <- (Nz*Wdg)^0.49

  term1 <- Sw * Wp

  y <- fact1*fact2*fact3*fact4*fact5 + term1
  return(y)
}


quackquack_wingweight <- function(){
  out <- list(input_dim=10)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(150, 220, 6, -10, 16, 0.5, 0.08, 2.5, 1700, 0.025),
              c(200, 300, 10, 10, 45, 1.0, 0.18, 6.0, 2500, 0.08))
  rownames(RR) <- c("Sw", "Wfw", "A", "Lam", "q", "lam", "tc", "Nz", "Wdg", "Wp")
  out$input_range <- RR
  return(out)
}

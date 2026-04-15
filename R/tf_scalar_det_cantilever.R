#' @name cantilever
#' @rdname cantilever
#'
#' @title The Cantilever Beam Function
#' @description
#' Dimensions: 6. The Cantilever Beam has two outputs: stress (S) and displacement (D). This can also be treated as a single function with bivariate output.
#'
#' @param x Inputs of dimension (at least) 5. See below for details.
#' @param scale01 (Ignored here) When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param L Length of beam. Default is 100 inches.
#' @param D0 Displacement tolerance. Default is 2.2535 inches (Constraint: D < D_0).
#' @return A scalar response
#' @details For details on the Cantilever Beam, see \href{https://www.sfu.ca/~ssurjano/canti.html}{the VLSE}.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Sues, Robert, Mohammad Aminpour, and Youngwon Shin. "Reliability based MDO for aerospace systems." 19th AIAA Applied Aerodynamics Conference. 2000.
#'
#'Wu, Y-T., et al. "Safety-factor based approach for probability-based design optimization." 19th AIAA applied aerodynamics conference. 2001.
NULL


#' @rdname cantilever
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, friedman, scale01=TRUE)
cantilever_D <- function(x, scale01=TRUE, L=100, D0=2.2535){
  if(scale01){
    RR <- rbind(40000 + 2000*c(-4, 4),
                2.9e7 + 1.45e6*c(-4, 4),
                500 + 100*c(-4, 4),
                1000 + 100*c(-4, 4),
                4 + 0.25*c(-4, 4),
                2 + 0.25*c(-4, 4))
    x[1:6] <- x[1:6]*(RR[,2] - RR[,1]) + RR[,1]
  }

  R <- x[1]
  E <- x[2]
  X <- x[3]
  Y <- x[4]
  w <- x[5]
  t <- x[6]

  term1 <- 4*L^3/(E*w*t)
  term2 <- sqrt(Y^2/t^4 + X^2/w^4)
  res <- term1 * term2
  res <- min(res, D0)
  return(res)
}

#' @rdname cantilever
#' @export
cantilever_S <- function(x, scale01=TRUE){
  if(scale01){
    RR <- rbind(40000 + 2000*c(-4, 4),
                2.9e7 + 1.45e6*c(-4, 4),
                500 + 100*c(-4, 4),
                1000 + 100*c(-4, 4),
                4 + 0.25*c(-4, 4),
                2 + 0.25*c(-4, 4))
    x[1:6] <- x[1:6]*(RR[,2] - RR[,1]) + RR[,1]
  }

  R <- x[1]
  E <- x[2]
  X <- x[3]
  Y <- x[4]
  w <- x[5]
  t <- x[6]

  res <- 600*Y/(w*t^2) + 600*X/(w^2*t)
  res <- min(res, R)
  return(res)
}

quackquack_cantilever_D <- function(){
  out <- list(input_dim=6)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- rbind(40000 + 2000*c(-4, 4),
              2.9e7 + 1.45e6*c(-4, 4),
              500 + 100*c(-4, 4),
              1000 + 100*c(-4, 4),
              4 + 0.25*c(-4, 4),
              2 + 0.25*c(-4, 4))

  rownames(RR) <- c("R", "E", "X", "Y", "w", "t")
  out$input_range <- RR
  return(out)
}

quackquack_cantilever_S <- function(){
  out <- list(input_dim=6)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- rbind(40000 + 2000*c(-4, 4),
              2.9e7 + 1.45e6*c(-4, 4),
              500 + 100*c(-4, 4),
              1000 + 100*c(-4, 4),
              4 + 0.25*c(-4, 4),
              2 + 0.25*c(-4, 4))

  rownames(RR) <- c("R", "E", "X", "Y", "w", "t")
  out$input_range <- RR
  return(out)
}

#' @title Steel Column
#'
#' @description
#' Dimension 9. Models the "reliability" of a steel column.
#'
#' @param x Inputs of dimension (at least) 5. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval.
#' @param b default parameter
#' @param d default parameter
#' @param h default parameter
#' @details This function is often used to explore the trade-off between cost and reliability of a steel column. The cost is \code{b*d + 5*h} which are parameters giving the mean flange breadth, flange thickness, and profile height, respectively. Non-normal distributions are often used for the priors.
#' For details, see \href{https://www.sfu.ca/~ssurjano/steelcol.html}{the VLSE}.
#' @references
#' Kuschel, Norbert, and Rüdiger Rackwitz. "Two basic problems in reliability-based structural optimization." Mathematical Methods of Operations Research 46 (1997): 309-333.
#' @rdname steel_column
#' @examples
#' X <- lhs::randomLHS(50, 7)
#' y <- apply(X, 1, steel_column, scale01=TRUE)
#' @export
steel_column <- function(x, scale01=TRUE, b=300, d=20, h=300){
  if(scale01){
    RR <- rbind(400 + 35*c(-4, 4),
                500000 + 50000*c(-4, 4),
                600000 + 90000*c(-4, 4),
                600000 + 90000*c(-4, 4),
                b + 3*c(-4, 4),
                d + 2*c(-4, 4),
                h + 5*c(-4, 4),
                30 + 10*c(-2.999, 4),
                210000 + 4200*c(-4, 4))
    x[1:9] <- x[1:9]*(RR[,2] - RR[,1]) + RR[,1]
  }

  Fs <- x[1]
  P1 <- x[2]
  P2 <- x[3]
  P3 <- x[4]
  B  <- x[5]
  D  <- x[6]
  H  <- x[7]
  F0 <- x[8]
  E  <- x[9]

  Eb <- base::pi^2*E*B*D*H^2/(2*(7500)^2)
  P <- P1 + P2 + P3
  term1 <- 1/(2*B*D)
  term2 <- F0*Eb/(B*D*H*(Eb-P))

  res <- Fs - P*(term1 + term2)
  return(res)
}



quackquack_steel_column <- function(){
  out <- list(input_dim=9)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  RR <- rbind(400 + 35*c(-4, 4),
              500000 + 50000*c(-4, 4),
              600000 + 90000*c(-4, 4),
              600000 + 90000*c(-4, 4),
              300 + 3*c(-4, 4),
              20 + 2*c(-4, 4),
              300 + 5*c(-4, 4),
              30 + 10*c(-2.999, 4),
              210000 + 4200*c(-4, 4))

  rownames(RR) <- c("Fs", "P1", "P2", "P3", "B", "D", "H", "F0", "E")
  out$input_range <- RR
  return(out)
}

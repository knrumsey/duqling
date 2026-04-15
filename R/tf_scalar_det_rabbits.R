#' The Rabbits Function (Logistic Growth)
#'
#' Dimensions 3. Classic logistic growth with fixed carrying capacity K=1.
#'
#' @param x Inputs of dimension (at least) 3. x[1]=P0 (initial pop), x[2]=t (time), x[3]=r (growth rate).
#' @param scale01 When TRUE, all inputs are expected to be on the unit interval.
#' @return Population size at time t.
#' @details
#' Logistic growth model:
#' \deqn{P(t) = \frac{P_0 e^{r t}}{1 + P_0 (e^{r t} - 1)}}
#' with P0 in [0,1], t in [0,1], r in [0.5,3].
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(3*n), nrow=n)
#' y <- apply(x, 1, rabbits)
rabbits <- function(x, scale01=TRUE){
  RR <- cbind(c(0, 0, 0.5),
              c(1, 1, 3))
  if(scale01){
    x[1:3] <- x[1:3]*(RR[,2] - RR[,1]) + RR[,1]
  }
  P0 <- x[1]
  t  <- x[2]
  r  <- x[3]
  res <- (P0 * exp(r*t)) / (1 + P0 * (exp(r*t) - 1))
  return(res)
}

#' @export
quackquack_rabbits <- function(){
  out <- list(input_dim=3)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  RR <- cbind(c(0,    0,   0.5),
              c(1,    1,   3))
  rownames(RR) <- c("P0", "t", "r")
  out$input_range <- RR
  return(out)
}

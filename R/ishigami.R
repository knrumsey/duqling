#' @title Ishigami Function
#'
#' @param x Inputs of dimension (at least) 3. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval.
#' @param ab Hyperparameters to the Ishigami function
#' @details Often used as an example, because it exhibits strong nonlinearity and nonmonotonicity. It has a peculiar dependence on x3. By default, the distribution of the inputs are x_i ~ Uniform(-pi, pi)
#' @references
#' Ishigami, T., & Homma, T. (1990 December). An importance quantification technique in uncertainty analysis for computer models. In Uncertainty Modeling and Analysis, 1990.
#' @rdname ishigami
#' @examples
#' X <- lhs::randomLHS(50, 3)
#' y <- apply(X, 1, ishigami, scale01=TRUE)
#' @export
ishigami <- function(x, scale01=FALSE, ab=c(7, 0.1)){
  if(scale01){
    RR <- cbind(rep(-base::pi, 3), rep(base::pi, 3))
    x[1:3] <- x[1:3]*(RR[,2] - RR[,1]) + RR[,1]
  }
  res <- sin(x[1]) + ab[1]*sin(x[2])^2 + ab[2]*x[3]^4*sin(x[1])
  return(res)
}


quackquack_ishigami <- function(){
  out <- list(input_dim=3)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  out$stochastic <- "n"

  RR <- cbind(rep(-base::pi, 3), rep(base::pi, 3))
  rownames(RR) <- c("x1", "x2", "x3")
  out$input_range <- RR
  return(out)
}

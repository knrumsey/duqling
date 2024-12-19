#' @name dms
#' @rdname dms
#'
#' @title Denison, Mallick and Smith Bivariate Test Functions
#'
#' @param x Inputs of dimension (at least) 8. See below for details.
#' @param scale01 (No effect here) When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @details The five test functions from Denison, Mallick and Smith (1998) are referred to as
#' \enumerate{
#'    \item Simple
#'    \item Radial
#'    \item Harmonic
#'    \item Additive
#'    \item Complex interaction
#' }
#' @references
#' Denison, David GT, Bani K. Mallick, and Adrian FM Smith. "Bayesian mars." Statistics and Computing 8.4 (1998): 337-346.
NULL

#' @rdname dms
#' @examples
#' n <- 100 #Number of observations
#' p <- 4   #Number of variables (beyond p = 2, variables are inert)
#' X <- matrix(runif(n*p), nrow=n)
#' y <- apply(X, 1, dms_simple)
#' @export
dms_simple <- function(x, scale01=TRUE) 10.391*((x[1]-0.4)*(x[2]-0.6) + 0.36)

#' @rdname dms
#' @export
dms_radial <- function(x, scale01=TRUE){ r <- (x[1]-0.5)^2 + (x[2] - 0.5)^2; 24.234*(r*(0.75 - r))}

#' @rdname dms
#' @export
dms_harmonic <- function(x, scale01=TRUE){ xx1 <- x[1] - 0.5; xx2 <- x[2] - 0.5; 42.659*(0.1 + xx1*(0.05 + xx1^4 - 10*xx1^2*xx2^2 + 5*xx2^4))}

#' @rdname dms
#' @export
dms_additive <- function(x, scale01=TRUE) 1.3356*(1.5*(1-x[1]) + exp(2*x[1] - 1)*sin(3*pi*(x[1] - 0.6)^2) + exp(3*(x[2]-0.5))*sin(4*pi*(x[2] - 0.9)^2))

#' @rdname dms
#' @export
dms_complicated <- function(x, scale01=TRUE) 1.9*(1.35 + exp(x[1])*sin(13*(x[1]-0.6)^2)*exp(-x[2])*sin(7*x[2]))


quackquack_dms_simple <- function(){
  out <- list(input_dim=2)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(0, 0),
              c(1, 1))
  rownames(RR) <- c("x1", "x2")
  out$input_range <- RR

  return(out)
}
quackquack_dms_radial <- quackquack_dms_simple
quackquack_dms_harmonic <- quackquack_dms_simple
quackquack_dms_additive <- quackquack_dms_simple
quackquack_dms_complicated <- quackquack_dms_simple



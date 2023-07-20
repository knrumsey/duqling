#' The Vinet Equation of State
#'
#' Dimensions 3. The Pressure as a function of density.
#'
#' @param x Inputs of dimension (at least) 3. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param density a vector of densities to evaluate.
#' @return Pressure
#' @details For details on the Vinet EOS, see Vinet (1989) or Brown & Hund (2018).
#' The parameter ranges correspond to a range of reasonable values for Tantalum.
#' \describe{
#'   \item{x1 = B0}{Bulk modulus (GPa), [195.6, 223.8]}
#'   \item{x2 = B0p}{1st derivative of B0, [2.9, 4.9]}
#'   \item{x3 = rho0}{initial density (g / cm^3), [16.15, 16.95]}
#' }
#' For a univariate version of this function, see \code{\link[duqling]{pollutant_uni}}
#' @references
#' Vinet, Pascal, et al. "Universal features of the equation of state of solids." Journal of Physics: Condensed Matter 1.11 (1989): 1941.
#'
#' Brown, Justin L., and Lauren B. Hund. "Estimating material properties under extreme conditions by using Bayesian model calibration with functional outputs." Journal of the Royal Statistical Society Series C: Applied Statistics 67.4 (2018): 1023-1045
#'
#' Rumsey, Kellin, et al. "Dealing with measurement uncertainties as nuisance parameters in Bayesian model calibration." SIAM/ASA Journal on Uncertainty Quantification 8.4 (2020): 1287-1309.
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(3*n), nrow=n)
#' y <- apply(x, 1, vinet, scale01=TRUE)
vinet <- function(x, scale01=FALSE,
                      density = seq(15.5, 17.5, by=0.1)){
  if(scale01){
    RR <- cbind(c(195.6, 2.9, 16.15),
                c(223.8, 4.9, 16.95))
    x[1:3] <- x[1:3]*(RR[,2] - RR[,1]) + RR[,1]
  }

  B0 <- x[1]
  B0p <- x[2]
  rho0 <- x[3]
  eta <- (rho0/density)^(1/3)
  term1 <- 3*B0*(1-eta)/eta^2
  term2 <- exp(1.5*(B0p-1)*(1-eta))
  y <- term1 + term2
  return(y)
}

quackquack_vinet <- function(){
  out <- list(input_dim=3)
  out$input_cat <- FALSE
  out$response_type <- "func"

  RR <- cbind(c(195.6, 2.9, 16.15),
              c(223.8, 4.9, 16.95))
  rownames(RR) <- c("B0", "B0p", "rho0")
  out$input_range <- RR
  return(out)
}

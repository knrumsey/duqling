#' Beam Deflection
#'
#' Dimensions 5. Measures the deflection of a length-L beam with a uniform load at location t away from origin.
#'
#' @param x Inputs: Uniform load density (P, N/m), Elastic Modulus (E, Mpa), beam length (L, m), beam width (w, m), beam thickness (h, m)
#' @param scale01  When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param p Vector of numbers between 0 and 1. p*L is location from the origin (left most side of beam) at which deflection is measured.
#' @return The "output" vector is given by \code{-P/(24EI)ell(ell^3 - 2*L*ell^2 + L^3)} where \code{I = w*h^3/12} and \code{ell = p*L}
#' @details A modified version of the beam deflection problem from the intro of Joseph (2024)
#' @references Joseph, Roshan. "Rational Kriging". JASA, (2024)
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(3*n), nrow=n)
#' y <- apply(x, 1, beam_deflection)
beam_deflection <- function(x, scale01=TRUE, p=seq(0, 1, length.out=20)){
  if(scale01){
    RR <- cbind(c(1.9e-4, 7000, 0.5, 0.076, 0.013),
                c(1.5e6, 14000, 1.5, 0.305, 0.152))
    x[1:5] <- x[1:5]*(RR[,2] - RR[,1]) + RR[,1]
  }

  P <- x[1]     # Load density (N/m)
  E <- x[2]*1e6 # Elastic modulus (MPa)
  L <- x[3]     # Beam length (m)
  w <- x[4]     # Beam width (m)
  h <- x[5]     # Beam thickness (m)

  I <- w*h^3/12 # Area Moment of Inertia
  xx <- p*L
  y <- -P/(24*E*I)*xx*(xx^3 - 2*L*xx^2 + L^3)
  return(y)
}

quackquack_beam_deflection <- function(){
  out <- list(input_dim=5)
  out$input_cat <- FALSE
  out$response_type <- "func"

  RR <- cbind(c(1.9e-4, 7000, 0.5, 0.076, 0.013),
              c(1.5e6, 14000, 1.5, 0.305, 0.152))
  rownames(RR) <- c("P", "E", "L", "w", "h")
  out$input_range <- RR
  return(out)
}



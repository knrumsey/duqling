#' @title Sulfur
#'
#' @description
#' Dimension 9. Models the radiative forcing of sulfur with uncertain inputs. High degree of interaction (but additive in log space).
#'
#' @param x Inputs of dimension (at least) 5. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval.
#' @param S0 Solar constant
#' @param A Surface area of the earth.
#' @details This function models the radiative forcing of sulfur by sulfate aerosols, in W/m^2. Note that all priors are lognormal.
#' For details, see \href{https://www.sfu.ca/~ssurjano/sulf.html}{the VLSE}. NOTE: There is a problem with dimensional analysis of the Tatang paper. There is a hidden (or missing) W in the denominator. As such, the results do not match the paper perfectly. Use this function at your own risk.
#' @references
#' Tatang, Menner A., et al. "An efficient method for parametric uncertainty analysis of numerical geophysical models." Journal of Geophysical Research: Atmospheres 102.D18 (1997): 21925-21932
#'
#' Charlson, Robert J., et al. "Climate forcing by anthropogenic aerosols." Science 255.5043 (1992): 423-430.
#' @rdname sulfur
#' @examples
#' X <- lhs::randomLHS(50, 7)
#' y <- apply(X, 1, sulfur, scale01=TRUE)
#' @export
sulfur <- function(x, scale01=TRUE, S0=1366, A=5.1e14){
  if(scale01){
    RR <- rbind(0.76 * 1.2^c(-3, 3),
                0.39* 1.1^c(-3, 3),
                0.85* 1.1^c(-3, 3),
                0.3* 1.3^c(-3, 3),
                5.0* 1.4^c(-3, 3),
                1.7* 1.2^c(-3, 3),
                71* 1.15^c(-3, 3),
                0.5* 1.5^c(-3, 3),
                5.5* 1.5^c(-3, 3))

    x[1:9] <- x[1:9]*(RR[,2] - RR[,1]) + RR[,1]
  }

  Tr <- x[1]
  Ac_c <- x[2]
  Rs_c <- x[3]
  beta <- x[4]
  psi_e <- x[5]
  f_psi <- x[6]
  Q <- x[7]
  Y <- x[8]
  L <- x[9]

  # Constants
  #S0 <- 1366 # solar constant
  #A <- 5.1e14 # surface area of the earth

  res <- -1/2*S0^2*Ac_c*Tr^2*Rs_c^2*beta*psi_e*f_psi*3*Q*Y*L/A * 10^12 / 365
  res <- res / 1000 # Just an attempt to match the origianl paper. See note in documentation.
  return(res)
}


quackquack_sulfur <- function(){
  out <- list(input_dim=9)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  RR <- rbind(0.76 * 1.2^c(-3, 3),
              0.39* 1.1^c(-3, 3),
              0.85* 1.1^c(-3, 3),
              0.3* 1.3^c(-3, 3),
              5.0* 1.4^c(-3, 3),
              1.7* 1.2^c(-3, 3),
              71* 1.15^c(-3, 3),
              0.5* 1.5^c(-3, 3),
              5.5* 1.5^c(-3, 3))

  rownames(RR) <- c("Tr", "Ac_c", "Rs_c", "beta", "psi_e", "f_psi", "Q", "Y", "L")
  out$input_range <- RR
  return(out)
}

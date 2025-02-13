#' @title Crater Depth
#'
#' @param x Inputs of dimension (at least) 7. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval.
#' @details A physics-based function for predicting crater depth resulting from high-velocity impacts, such as those experienced by the Space Shuttle Columbia during reentry.
#' Input variables include length of projectile (in), diameter of projectile (in), density of projectile (lb/in^3), normal velocity (in/s), velocity required to break through tile coating (in/s), compressive tile strength (psi), and tile density (lb/in^3).
#' The \code{const} and \code{power} inputs default to 0.0195 and 0.45 per the "original R&C formula" but can be varied; Stellingwerf et al set these to 0.67 and 0.0086.
#' @references
#' Stellingwerf, R., Robinson, J., Richardson, S., Evans, S., Stallworth, R., & Hovater, M. (2004). Foam-on-tile impact modeling for the STS-107 investigation. In 45th AIAA/ASME/ASCE/AHS/ASC Structures, Structural Dynamics & Materials Conference (p. 1881).
#' @rdname crater
#' @examples
#' X <- lhs::randomLHS(50, 7)
#' y <- apply(X, 1, crater, scale01=TRUE)
#' @export
crater <- function(x, scale01=TRUE, const=0.0195, power=0.45){
  if(scale01){
    RR <- rbind(c(0.01, 20), c(0.01, 6), c(.0001, .04), c(730, 3200), c(1500,2500), c(25, 77), c(.005, .0055))
    x[1:7] <- x[1:7]*(RR[,2] - RR[,1]) + RR[,1]
  }

  # returns penetration depth (in)
  L = x[1] # length of projectile (in)
  d = x[2] # diameter of projectile (in)
  rho_p = x[3] # density of projectile (lb/in^3)
  V = x[4] # normal velocity (in/s)
  V_star = x[5] # velocity required to break through the tile coating (in/s)
  S_t = x[6] # crompressive strength of tile (psi)
  rho_t = x[7] # density of tile (lb/in^3)

  res = const * (L/d)^power * d * rho_p^0.27 * max(V - V_star, 0)^(2/3) * S_t^-0.25 * rho_t^(-1/6)
  return(res)
}


quackquack_crater <- function(){
  out <- list(input_dim=7)
  out$input_cat <- FALSE
  out$response_type <- "uni"
  out$stochastic <- "n"

  RR <- rbind(c(0.01, 20), c(0.01, 6), c(.0001, .04), c(730, 3200), c(1500,2500), c(25, 77), c(.005, .0055))
  rownames(RR) <- c("L", "d", "rho_p", "V", "V_hat", "S", "rho_t")
  out$input_range <- RR
  return(out)
}

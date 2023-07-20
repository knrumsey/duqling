#' Synthetic Twin Galaxy Function
#'
#' Dimensions 2. A synthetic computer model from Rumsey et al (2022) which was designed to mimic the challenges and features of the E3SM climate model. The name is derived purely from the appearance of the computer model when plotting.
#'
#' @param x Inputs of dimension (at least) 8. See below for details.
#' @param scale01  When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param fixed_coeff Logical. See \code{duqling::ripples} for details.
#' @param seed Optional. See \code{duqling::ripples} for details.
#' @return Univariate response representing small scale structure
#' @details Function with long range and small scale structures and prominent continent-esque features (the twin galaxies). See Rumsey et al (2022) for details along with the \code{duqling::lim_polyonmial}, \code{duqling::grlee2} and \code{duqling::ripples} functions. Inputs are called lon and lat with ranges (0, 360) and (-90, 90) respectively, due to the originiation of this function with climate models.
#' @references
#' Rumsey, Kellin, et al. "A hierarchical sparse Gaussian process for in situ inference in expensive physics simulations." Applications of machine learning 2022. Vol. 12227. SPIE, 2022.
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(2*n), nrow=n)
#' y <- apply(x, 1, twin_galaxies)
twin_galaxies <- function(x, scale01=FALSE, fixed_coeff=TRUE, seed=NULL){
  if(!scale01){
    x[1] <- x[1]/360
    x[2] <- (x[2] + 90)/180
  }

  y <- 22/40*lim_polynomial(x, scale01=TRUE)
  y <- y + 5*grlee2(x, scale01=TRUE)
  y <- y + ripples(x, scale01=TRUE, fixed_coeff=fixed_coeff, seed=seed)
  return(y)
}

quackquack_twin_galaxies <- function(){
  out <- list(input_dim=2)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(0,-90), c(360,90))
  rownames(RR) <- c("lon", "lat")
  out$input_range <- RR

  return(out)
}

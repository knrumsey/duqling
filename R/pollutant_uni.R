#' Univariate Pollutant Spill in a Channel
#'
#' Dimensions 4. The \code{\link[duqling]{pollutant}} function evaluated at time 30 and space = 2.5
#'
#' @param x Inputs of dimension (at least) 4. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param space single location along the channel (default 2.5)
#' @param time single time of measurement (default 30)
#' @return C(x) is the concentration of the pollutant at the space-time vector (s, t)
#' @details For details on the pollutant function, see \href{https://www.sfu.ca/~ssurjano/environ.html}{the VLSE}.
#' Parameter ranges:
#' \describe{
#'   \item{x1 = M}{mass of pollutant spilled at each location, [7, 13]}
#'   \item{x2 = D}{diffusion rate in the channel, [0.02, 0.12]}
#'   \item{x3 = L}{location of the second spill, [0.01, 3]}
#'   \item{x4 = tau}{time of the second spill, [30.01, 30.295]}
#' }
#' For the spatio-temporal version of this model, see \code{\link[duqling]{pollutant}}
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Bliznyuk, N., Ruppert, D., Shoemaker, C., Regis, R., Wild, S., & Mugunthan, P. (2008). Bayesian calibration and uncertainty analysis for computationally expensive models using optimization and radial basis function approximation. Journal of Computational and Graphical Statistics, 17(2).
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, pollutant_uni, scale01=TRUE)
pollutant_uni <- function(x, scale01=FALSE,
                      space=2.5,
                      time=30){
  as.numeric(pollutant(x, scale01, space, time))
}

quackquack_pollutant_uni <- function(){
  out <- list(input_dim=4)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(7, 0.02, 0.01, 30.01),
              c(13, 0.12, 3, 30.295))
  rownames(RR) <- c("M", "D", "L", "tau")
  out$input_range <- RR
  return(out)
}

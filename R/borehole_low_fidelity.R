#' The Low Fidelity Borehole Function
#'
#' Dimensions 8. Low fidelity version of the borehole function (Xiong 2013). See \code{\link[duqling]{borehole}} for more details.
#'
#' @param x Inputs of dimension (at least) 8. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE

#' @return Flow through a borehole.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Xiong, S., Qian, P. Z., & Wu, C. J. (2013). Sequential design and analysis of high-accuracy and low-accuracy computer codes. Technometrics, 55(1), 37-46.
#' @export
borehole_low_fidelity <- function(x, scale01=FALSE){
  borehole(x, scale01, adjust_fidelity=1)
}

quackquack_borehole_low_fidelity <- function(){
  out <- list(input_dim=8)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(c(0.05, 100, 63070, 990, 63.1,  700, 1120, 9855),
              c(0.15, 50000, 115600, 1110, 116, 80, 1680, 12045))
  rownames(RR) <- c("rw", "r", "Tu", "Hu", "Tl", "Hl", "L", "Kw")
  out$input_range <- RR
  return(out)
}

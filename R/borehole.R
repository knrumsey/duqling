#' The Borehole Function
#'
#' Dimensions 8. The Borehole function models water flow through a borehole. Its simplicity and quick evaluation makes it a commonly used function for testing a wide variety of methods in computer experiments.
#'
#' @param x Inputs of dimension (at least) 8. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range.
#' @param adjust_fidelity Default value of 0 corresponds to the usual borehole function. Value of 1 corresponds to the low-fidelity version described in the VLSE (Xiong 2013).
#' @return Flow through a borehole.
#' @details For details on the borehole function, see \href{https://www.sfu.ca/~ssurjano/borehole.html}{the VLSE}.
#' Parameter ranges:
#' \describe{
#'   \item{x1 = rw}{radius of borehole (m), [0.05, 0.15]}
#'   \item{x2 = r}{radius of influence (m), [100, 50000]}
#'   \item{x3 = Tu}{transmissivity of borehole (m^2/yr), [63070, 115600]}
#'   \item{x4 = Hu}{potentiometric head of upper aquifer (m), [990, 1110]}
#'   \item{x5 = Tl}{transmissivity of lower aquifer (m2/yr), [63.1, 116]}
#'   \item{x6 = Hl}{potentiometric head of lower aquifer (m), [700, 820]}
#'   \item{x7 = L}{length of borehole (m), [1120, 1680]}
#'   \item{x8 = Kw}{hydraulic conductivity of borehole (m/yr), [9855, 12045]}
#' }
#' For a more non-linear and non-additive function (Xiong 2013), the range of Kw may be changed to [1500, 15 000]. Fidelity can be adjusted to match the low-fidelity borehole function of Xiong 2013.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Harper, W. V., & Gupta, S. K. (1983). Sensitivity/uncertainty analysis of a borehole scenario comparing Latin Hypercube Sampling and deterministic sensitivity approaches (No. BMI/ONWI-516). Battelle Memorial Inst., Columbus, OH (USA). Office of Nuclear Waste Isolation.
#'
#' Xiong, S., Qian, P. Z., & Wu, C. J. (2013). Sequential design and analysis of high-accuracy and low-accuracy computer codes. Technometrics, 55(1), 37-46.
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, borehole, scale01=TRUE)
borehole <- function(x, scale01=TRUE, adjust_fidelity = 0){
  if(scale01){
    RR <- cbind(c(0.05, 100, 63070, 990, 63.1,  700, 1120, 9855),
                   c(0.15, 50000, 115600, 1110, 116, 820, 1680, 12045))
    x[1:8] <- x[1:8]*(RR[,2] - RR[,1]) + RR[,1]
  }

  rw <- x[1]
  r  <- x[2]
  Tu <- x[3]
  Hu <- x[4]
  Tl <- x[5]
  Hl <- x[6]
  L  <- x[7]
  Kw <- x[8]

  frac1 <- (2 * pi - adjust_fidelity*1.283185)  * Tu * (Hu-Hl)
  frac2a <- 2*L*Tu / (log(r/rw)*rw^2*Kw)
  frac2b <- Tu / Tl
  frac2 <- log(r/rw) * (1+ adjust_fidelity/2 +frac2a+frac2b)

  y <- frac1 / frac2
  return(y)
}

quackquack_borehole <- function(){
  out <- list(input_dim=8)
  out$input_cat <- FALSE
  out$response_type <- "uni"


  RR <- cbind(c(0.05, 100, 63070, 990, 63.1,  700, 1120, 9855),
              c(0.15, 50000, 115600, 1110, 116, 80, 1680, 12045))
  rownames(RR) <- c("rw", "r", "Tu", "Hu", "Tl", "Hl", "L", "Kw")
  out$input_range <- RR
  return(out)
}

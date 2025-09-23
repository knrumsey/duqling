#' Here we collect some test functions with categorical inputs.
#' For ease of use and consistency with the systematic behavior of the
#' future function: run_sim_study_cat(), we re-code all categorical variables
#' using binary coding. The metadata fields $categorical_map and $num_classes
#' are enough to do this on the original scale, if desired.

cat_map <- function(bits, num_classes, clamp=TRUE) {
  # Convert "0"/"1" to integers
  b <- as.integer(bits)

  # Decode binary vector (most significant bit first)
  idx <- sum(b * 2^((length(b)-1):0)) + 1

  # Clamp to the maximum number of classes
  if (idx > num_classes && clamp) idx <- num_classes

  return(idx)
}

#' @title Moon (2010) mixed function
#'
#' @description Dimensions 2 continuous, 1 categorical. A test function with continuous and categorical inputs, designed for sensitivity analysis and screening
#'
#' @param x Quantitative inputs of dimension 2.
#' @param z Binary-coded categorical inputs of dimension 1
#' @param scale01 When \code{TRUE}, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @return A scalar response.
#' @details For details, see \href{https://www.sfu.ca/~ssurjano/moon10mix.html}{the VLSE}.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Moon, H. (2010). Design and Analysis of Computer Experiments for Screening Input Variables (Doctoral dissertation, Ohio State University).
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, pollutant, scale01=TRUE)
#' @export
moon3_cat <- function(x, z, scale01=TRUE){
  # Categorical information
  input_cat_dim <- 1
  num_classes <- c(2)
  vals <- as.integer(as.character(z))
  for(i in seq_len(input_cat_dim)){
    if(!(vals[i] %in% 1:num_classes[i])) stop("Categorical inputs must be non-negative integers")
  }

  res <- (x[1] + x[2]) * (w[1] == 1) + 3 * x[1] * (w[1] == 2)
  return(res)
}

quackquack_moon3_cat <- function(){
  out <- list()

  RR <- cbind(c(0,0), c(1,1))
  rownames(RR) <- c("x1", "x2")

  # Standard fields
  out$input_dim <- 2
  out$response_type <- "univariate"
  out$stochastic <- FALSE
  out$input_range <- RR

  # Categorical fields
  out$input_cat <- TRUE
  out$input_cat_dim <- 1
  out$num_classes <- c("z1"=2)

  return(out)
}


#' @title Mixed Inputs Borehole Function
#'
#' @description Dimensions: 4 continuous, 3 categorical. A test function with continuous and categorical inputs.
#'
#' @param x Quantitative inputs of dimension 2.
#' @param z Binary-coded categorical inputs of dimension 1
#' @param scale01 When \code{TRUE}, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param log Should the response be log-transformed (as in Zhang et al (2021)).
#' @param case Corresponds to the cases in Table 2 of Zhang et al (2021). The number of qualitative levels is equal to case1: 27, case2: 40, case3: 60, case4: 80.
#' @return A scalar response.
#' @details For details, see \href{https://www.sfu.ca/~ssurjano/moon10mix.html}{the VLSE}.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Zhou, Q., Qian, P. Z., & Zhou, S. (2011). A simple approach to emulation for computer models with qualitative and quantitative factors. Technometrics, 53(3).
#'
#' Zhang, Q., Chien, P., Liu, Q., Xu, L., & Hong, Y. (2021). Mixed-input Gaussian process emulators for computer experiments with a large number of categorical levels. Journal of Quality Technology, 53(4), 410-420.
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, pollutant, scale01=TRUE)
#' @export
borehole_cat <- function(x, z, scale01=TRUE, log=TRUE, case=1){
  # Categorical information
  input_cat_dim <- 3
  num_classes <- switch(case,
                         "1"=c(3,3,3),
                         "2"=c(2,2,10),
                         "3"=c(2,2,15),
                         "4"=c(2,2,20))
  vals <- as.integer(as.character(z))
  for(i in seq_len(input_cat_dim)){
    if(!(vals[i] %in% 1:num_classes[i])) stop("Categorical inputs must be non-negative integers")
  }

  # Categorical (from Table 1 in Zhang et al 2021)
  Hl <- 0       # Only Hu-Hl matters
  r  <- (z[1] / num_classes[1]) * (50000 - 100) + 100
  Hu <- (z[2] / num_classes[2]) * (390 - 290) + 290
  Kw <- (z[3] / num_classes[3]) * (12045 - 9855) + 9855

  # Numerical
  rw <- x[1]
  Tu <- x[2]
  Tl <- x[3]
  L  <- x[4]

  if(scale01){
    # From borehole
    RR <- cbind(c(0.05, 100, 63070, 990, 63.1, 700, 1120, 9855),
                c(0.15, 50000, 115600, 1110, 116, 820, 1680, 12045))
    # Just the numeric ranges
    RR <- RR[c(1,3,5,7),]
    x[1:4] <- x[1:4]*(RR[,2] - RR[,1]) + RR[,1]
  }

  xx <- c(rw, r, Tu, Hu, Tl, Hl, L, Kw)
  res <- borehole(xx, scale01=FALSE)
  if(log) res <- log(res)
  return(res)
}

quackquack_borehole_cat <- function(){
  out <- list()

  RR <- cbind(c(0.05, 63070, 63.1, 1120),
              c(0.15, 115600, 116, 1680))
  rownames(RR) <- c("rw", "Tu", "Tl", "L")

  # Standard fields
  out$input_dim <- 2
  out$response_type <- "univariate"
  out$stochastic <- FALSE
  out$input_range <- RR

  # Categorical fields
  out$input_cat <- TRUE
  out$input_cat_dim <- 3
  out$num_classes <- c("r"=3, "dH"=3, "Kw"=3)

  return(out)
}

#' @title Mixed Inputs Borehole Function (Case 4)
#'
#' @description A wrapper for the \code{borehole_cat()} function with \code{case=4}; the 3 categorical cases have 80 levels.
#'
#' @param x Quantitative inputs of dimension 2.
#' @param z Binary-coded categorical inputs of dimension 1
#' @param scale01 When \code{TRUE}, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param log Should the response be log-transformed (as in Zhang et al (2021)).
#' @return A scalar response.
#' @details For details, see \href{https://www.sfu.ca/~ssurjano/moon10mix.html}{the VLSE}.
#' @references
#' Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.
#'
#' Zhou, Q., Qian, P. Z., & Zhou, S. (2011). A simple approach to emulation for computer models with qualitative and quantitative factors. Technometrics, 53(3).
#'
#' Zhang, Q., Chien, P., Liu, Q., Xu, L., & Hong, Y. (2021). Mixed-input Gaussian process emulators for computer experiments with a large number of categorical levels. Journal of Quality Technology, 53(4), 410-420.
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, pollutant, scale01=TRUE)
#' @export
borehole_cat80 <- function(x, z, scale01=TRUE, log=TRUE){
  borehole_cat(x, z, scale01, log, case=4)
}

quackquack_borehole_cat80 <- function(){
  out <- list()

  RR <- cbind(c(0.05, 63070, 63.1, 1120),
              c(0.15, 115600, 116, 1680))
  rownames(RR) <- c("rw", "Tu", "Tl", "L")

  # Standard fields
  out$input_dim <- 2
  out$response_type <- "univariate"
  out$stochastic <- FALSE
  out$input_range <- RR

  # Categorical fields
  out$input_cat <- TRUE
  out$input_cat_dim <- 3
  out$num_classes <- c("r"=2, "dH"=2, "Kw"=20)

  return(out)
}

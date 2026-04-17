#' 12D Turbulence-Inspired Toy Function
#'
#' A deterministic 12-dimensional toy model inspired by simple algebraic
#' eddy-viscosity ideas. This is not a true RANS closure model, but is meant
#' to mimic some of the nonlinear structure seen in turbulence surrogates:
#' wall damping, strain dependence, Reynolds-number effects, and dissipation.
#'
#' @param x Inputs of dimension at least 12.
#' @param scale01 Are inputs given on (0, 1) scale?
#' @return Scalar output.
#' @details
#' Inputs are interpreted as:
#' \describe{
#'   \item{x1}{log-Reynolds-number surrogate}
#'   \item{x2}{wall-distance surrogate}
#'   \item{x3}{strain-rate magnitude surrogate}
#'   \item{x4}{mixing length surrogate}
#'   \item{x5}{eddy-viscosity coefficient}
#'   \item{x6}{wall-damping coefficient}
#'   \item{x7}{pressure-gradient surrogate}
#'   \item{x8}{curvature/separation surrogate}
#'   \item{x9}{production coefficient}
#'   \item{x10}{dissipation coefficient}
#'   \item{x11}{anisotropy surrogate}
#'   \item{x12}{large-scale shear surrogate}
#' }
#'
#' @export
#' @examples
#' fname <- "turb12"
#' n <- 10
#' p <- quack(fname, verbose=FALSE)$input_dim
#' x <- matrix(runif(n * p), nrow = n)
#' y <- eval_duq(fname, x)
turb12 <- function(x, scale01 = TRUE) {
  if (scale01) {
    RR <- quackquack_turb12()$input_range
    x[1:12] <- x[1:12] * (RR[,2] - RR[,1]) + RR[,1]
  }

  logRe  <- x[1]
  ywall  <- x[2]
  S      <- x[3]
  ell    <- x[4]
  Cmu    <- x[5]
  Aplus  <- x[6]
  beta   <- x[7]
  chi    <- x[8]
  Cp     <- x[9]
  Cd     <- x[10]
  anis   <- x[11]
  shear  <- x[12]

  fw <- (1 - exp(-Aplus * ywall))^2
  prod <- Cp * abs(S)^1.5 * ell * (1 + 0.25 * tanh(beta))
  diss <- Cd * (1 + ell^2) * (1 + anis^2)
  nut <- Cmu * ell^2 * abs(S) * fw / (1 + abs(beta))

  sep_penalty <- 1 / (1 + beta^2)
  curvature_term <- tanh(chi) * log1p(logRe)
  shear_term <- shear^2 / (1 + shear^2 + anis^2)

  out <- sep_penalty * (prod / diss) +
    0.5 * nut / (1 + nut) +
    0.1 * curvature_term +
    0.2 * shear_term +
    0.05 * anis * fw

  out
}


#' @export
quackquack_turb12 <- function() {
  out <- list(input_dim = 12)
  out$input_cat <- FALSE
  out$response_type <- "uni"

  RR <- cbind(
    c(4.0,   0.001,  0.05,  0.01, 0.05,  2.0, -2.0, -3.0, 0.2, 0.2, 0.0, 0.1),
    c(8.0,   0.200, 10.00,  0.50, 0.30, 25.0,  2.0,  3.0, 2.0, 2.0, 2.0, 5.0)
  )
  rownames(RR) <- c(
    "logRe", "ywall", "S", "ell", "Cmu", "Aplus",
    "beta", "chi", "Cp", "Cd", "anis", "shear"
  )
  out$input_range <- RR
  out
}

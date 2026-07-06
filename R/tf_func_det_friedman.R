#' Functional Friedman Function
#'
#' Functional-output version of the Friedman function. The first input is treated
#' as the functional argument \code{u}; the remaining inputs are ordinary scalar
#' inputs.
#'
#' @param x Numeric vector of length 4. Inputs \code{x[1]}, ..., \code{x[4]}.
#'   All inputs are assumed to lie in \eqn{[0,1]}.
#' @param scale01 Logical. Included for consistency; inputs are already on
#'   \eqn{[0,1]} so no rescaling is applied.
#' @param u Vector of functional input locations in \eqn{[0,1]}.
#'
#' @return A numeric vector of function values evaluated at \code{u}.
#' @export
friedman_func <- function(x, scale01 = TRUE, u = seq(0, 1, length.out = 20)) {
  x2 <- x[1]
  x3 <- x[2]
  x4 <- x[3]
  x5 <- x[4]

  10 * sin(2 * pi * u * x2) +
    20 * (x3 - 0.5)^2 +
    10 * x4 +
    5 * x5
}


quackquack_friedman_func <- function() {
  out <- list(input_dim = 4)
  out$input_cat <- FALSE
  out$response_type <- "func"
  out$stochastic <- FALSE

  RR <- cbind(rep(0, 4), rep(1, 4))
  rownames(RR) <- c("x2", "x3", "x4", "x5")
  out$input_range <- RR

  return(out)
}


#' Low-Fidelity Functional Friedman Function
#'
#' Low-fidelity functional-output version of the Friedman function.
#'
#' The high-fidelity function is
#' \deqn{
#' f_H(u, x) =
#' 10\sin(2\pi u x_2) + 20(x_3 - 0.5)^2 + 10x_4 + 5x_5.
#' }
#'
#' The low-fidelity function is
#' \deqn{
#' f_L(u, x) = \delta f_H(u, x) + (1-\delta)g(u, x),
#' }
#' where
#' \deqn{
#' g(u, x) =
#' 10\sin(3\pi u x_2 - 0.5) + 20|x_3 - 0.5| + 10x_4.
#' }
#'
#' @param x Numeric vector of length 4. Inputs \code{x[1]}, ..., \code{x[4]}.
#'   All inputs are assumed to lie in \eqn{[0,1]}.
#' @param scale01 Logical. Included for consistency; inputs are already on
#'   \eqn{[0,1]} so no rescaling is applied.
#' @param u Vector of functional input locations in \eqn{[0,1]}.
#' @param delta Fidelity mixing parameter. Default is \code{0.8}.
#'
#' @return A numeric vector of function values evaluated at \code{u}.
#' @export
friedman_func_lf <- function(x, scale01 = TRUE,
                             u = seq(0, 1, length.out = 20),
                             delta = 0.5) {
  x2 <- x[1]
  x3 <- x[2]
  x4 <- x[3]
  x5 <- x[4]

  fH <- 10 * sin(2 * pi * u * x2) +
    20 * (x3 - 0.5)^2 +
    10 * x4 +
    5 * x5

  g <- 10 * sin(3 * pi * u * x2 - 0.5) +
    20 * abs(x3 - 0.5) +
    10 * x4

  delta * fH + (1 - delta) * g
}


quackquack_friedman_func_lf <- function() {
  out <- list(input_dim = 4)
  out$input_cat <- FALSE
  out$response_type <- "func"
  out$stochastic <- FALSE

  RR <- cbind(rep(0, 4), rep(1, 4))
  rownames(RR) <- c("x2", "x3", "x4", "x5")
  out$input_range <- RR

  return(out)
}

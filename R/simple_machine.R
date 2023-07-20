#' Simple Machine
#'
#' Dimensions 3. The simple machine computer model which is prominent in the Bayesian calibration literature. Functional response.
#'
#' @param x Inputs of dimension 1 to 3. Parameters are efficiency, friction and base-efficiency. If x has dimension less than 3, defaults are used for the other pars.
#' @param scale01  When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param effort Functional input. See details
#' @param friction friction value (only used if x is dim one or two)
#' @param base_eff base efficiency value (only used if x is dim one)
#' @return The "output" of the simple machine with parameters x at effort levels specified by \code{effort}.
#' @details The simple machine is given by
#'
#' \code{output = efficiency*effort/(1+effort*friction) + base efficiency}
#'
#' Usual parameter ranges are given by efficiency in (0, 3), friction in (0, 50) and base efficiency in (-1, 1). See also \code{\link[duqling]{simple_machine_cm}}.
#' @references
#' Brynjarsdóttir, Jenný, and Anthony OʼHagan. "Learning about physical parameters: The importance of model discrepancy." Inverse problems 30.11 (2014): 114007.
#'
#' Rumsey, Kellin N. Methods of uncertainty quantification for physical parameters. Diss. The University of New Mexico, 2020.
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(3*n), nrow=n)
#' y <- apply(x, 1, simple_machine)
simple_machine <- function(x, scale01=FALSE, effort=seq(1, 10, by=1), friction=1/10, base_eff=0){
  if(length(x) < 2){
    x[2] <- friction
  }
  if(length(x) < 3){
    x[3] <- base_eff
  }
  if(scale01){
    x[1] <- x[1]*3
    x[2] <- x[2]*50
    x[3] <- x[3]*2 - 1
  }

  y <- effort*x[1]/(1 + effort*x[2]) + x[3]
}


#' Simple Machine Computer Model (Piecewise Polynomial)
#'
#' Dimensions 5. An attempt to model the simple machine while recognizing the presence of friction but using an incorrect mechanism. Setting order = 1 returns the linear model from Brynjarsdottir and O'Hagan.
#'
#' @param x Inputs of dimension 1 to 3. Parameters are efficiency, friction and base-efficiency. If x has dimension less than 3, defaults are used for the other pars.
#' @param scale01  When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param effort Functional input. See details
#' @param order Default 3. The dimension of x becomes 3 and 1 when \code{order = 2} and \code{order = 1} respectively. See Rumsey (2020) eq. 4.33 for details.
#' @return The "output" of the simple machine with parameters x at effort levels specified by \code{effort}.
#' @details A piecewise polyonmial approximation to the \code{\link[duqling]{simple_machine}}. Parameters consist of (alpha, gamma1, beta1, gamma2, beta2)
#' @references
#' Brynjarsdóttir, Jenný, and Anthony OʼHagan. "Learning about physical parameters: The importance of model discrepancy." Inverse problems 30.11 (2014): 114007.
#'
#' Rumsey, Kellin N. Methods of uncertainty quantification for physical parameters. Diss. The University of New Mexico, 2020.
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(3*n), nrow=n)
#' y <- apply(x, 1, simple_machine)
simple_machine_cm <- function(x, scale01=FALSE, effort=seq(1, 10, by=1), order = 3){
  if(order == 1){
    x[2:5] <- c(10^9, 0, 10^9, 0)
  }
  if(order == 2){
    x[4:5] <- c(10^9, 0)
  }
  if(scale01){
    x[1] <- x[1]*3
    x[c(2,4)] <- x[c(2,4)]*10
    x[c(3,5)] <- x[c(3,5)]*3
  }

  y1 <- x[1]*effort * (effort < x[2])
  y2 <- (x[1]*x[2] + x[3]*(effort - x[2])) * (effort >= x[2] & effort < x[4])
  y3 <- (x[1]*x[2] + x[3]*(x[4] - x[2]) + x[5]*(effort - x[4])) * (effort >= x[4])
  return(y1 + y2 + y3)
}



quackquack_simple_machine_cm <- function(){
  out <- list(input_dim=5)
  out$input_cat <- FALSE
  out$response_type <- "func"

  RR <- cbind(rep(0, 5), c(3, 10, 3, 10, 3))
  rownames(RR) <- c("alpha", "gamma1", "beta1", "gamma2", "beta2")
  out$input_range <- RR

  return(out)
}


quackquack_simple_machine <- function(){
  out <- list(input_dim=3)
  out$input_cat <- FALSE
  out$response_type <- "func"

  RR <- cbind(c(0, 0, -1), c(3, 10, 1))
  rownames(RR) <- c("efficiency", "friction", "base_efficiency")
  out$input_range <- RR

  return(out)
}







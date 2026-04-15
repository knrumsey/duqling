#' Stochastic Black-Scholes formula
#'
#' Dimensions 6. Models the dynamics of a financial market (put and call prices)
#' under a stochastic model for the underlying asset.
#'
#' @param x Inputs of dimension (at least) 6. See below for details.
#' @param scale01 When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param time a vector of times (on a (0, 1) scale).
#' @return Put and call prices for an asset.
#' @details A stochastic version of the Black-Scholes model.
#' \describe{
#'   \item{x1 = sigma}{Volatility of the market, \eqn{\sigma \in [0.02, 0.6]}.}
#'   \item{x2 = T}{Time to maturity (years), \eqn{T \in [0.25, 2.0]}.}
#'   \item{x3 = K}{Strike price, \eqn{K \in [50, 150]}.}
#'   \item{x4 = r}{Risk-free interest rate, \eqn{r \in [0, 0.1]}.}
#'   \item{x5 = S0}{Initial underlying asset price, \eqn{S_0 \in [80, 120]}.}
#'   \item{x6 = mu}{Drift of the underlying asset under the real-world measure, \eqn{\mu \in [-0.1, 0.2]}.}
#' }
#' @references Black, Fischer (1976). The pricing of commodity contracts, Journal of Financial Economics, 3, 167-179.
#'
#' @rdname bs_call
#' @aliases bs_put
#' @export
#' @examples
#' n <- 10
#' x <- matrix(runif(8*n), nrow=n)
#' y <- apply(x, 1, bs_, scale01=TRUE)
bs_call <- function(x, scale01=TRUE,
                      time=seq(from=0, to=1, length.out=30)){
  if (scale01) {
    RR <- cbind(
      c(0.02, 0.25,  50, 0.00,  80, -0.10),  # lower bounds
      c(0.60, 2.00, 150, 0.10, 120,  0.20)   # upper bounds
    )
    x[1:6] <- x[1:6] * (RR[, 2] - RR[, 1]) + RR[, 1]
  }

  # Inputs
  sigma <- x[1]
  Tf    <- x[2]
  K     <- x[3]
  r     <- x[4]
  S0    <- x[5]
  mu    <- x[6]

  # Real time
  time <- sort(time) * Tf
  if(any(time < 0)) stop("time vector cannot be negative")

  # Asset price
  a <- (mu - sigma^2/2) * time
  b <- sigma * sqrt(time)
  z <- rnorm(length(time))
  St <- S0 * exp(a + b*z)

  # Get quantities
  tau <- Tf - time
  d_plus  <- (log(St / K) + (r + sigma^2/2) * tau) / (sigma * sqrt(tau))
  d_minus <- d_plus - sigma* sqrt(tau)

  call_price <- pnorm(d_plus) * St - pnorm(d_minus) * K * exp(-r*tau)
  if(any(tau == 0)){
    call_price[tau == 0] <- pmax(St-K, 0)
  }

  return(call_price)
}

#' @rdname bs_call
#' @export
bs_put <- function(x, scale01=TRUE,
                    time=seq(from=0, to=1, length.out=30)){
  if (scale01) {
    RR <- cbind(
      c(0.02, 0.25,  50, 0.00,  80, -0.10),  # lower bounds
      c(0.60, 2.00, 150, 0.10, 120,  0.20)   # upper bounds
    )
    x[1:6] <- x[1:6] * (RR[, 2] - RR[, 1]) + RR[, 1]
  }

  # Inputs
  sigma <- x[1]
  Tf    <- x[2]
  K     <- x[3]
  r     <- x[4]
  S0    <- x[5]
  mu    <- x[6]

  # Real time
  time <- sort(time) * Tf
  if(any(time < 0)) stop("time vector cannot be negative")

  # Asset price
  a <- (mu - sigma^2/2) * time
  b <- sigma * sqrt(time)
  z <- rnorm(length(time))
  St <- S0 * exp(a + b*z)

  # Get quantities
  tau <- Tf - time
  d_plus  <- (log(St / K) + (r + sigma^2/2) * tau) / (sigma * sqrt(tau))
  d_minus <- d_plus - sigma* sqrt(tau)

  call_price <- pnorm(d_plus) * St - pnorm(d_minus) * K * exp(-r*tau)
  if(any(tau == 0)){
    call_price[tau == 0] <- pmax(St-K, 0)
  }
  put_price <- K * exp(-r*tau) - St - call_price

  return(put_price)
}

quackquack_bs_call <- function(){
  out <- list(input_dim=6)
  out$input_cat <- FALSE
  out$response_type <- "func"

  RR <- cbind(
    c(0.02, 0.25,  50, 0.00,  80, -0.10),  # lower bounds
    c(0.60, 2.00, 150, 0.10, 120,  0.20)   # upper bounds
  )
  rownames(RR) <- c("sigma", "Tf", "K", "r", "S0", "mu")
  out$input_range <- RR
  return(out)
}

quackquack_bs_put <- function(){
  out <- list(input_dim=6)
  out$input_cat <- FALSE
  out$response_type <- "func"

  RR <- cbind(
    c(0.02, 0.25,  50, 0.00,  80, -0.10),  # lower bounds
    c(0.60, 2.00, 150, 0.10, 120,  0.20)   # upper bounds
  )
  rownames(RR) <- c("sigma", "Tf", "K", "r", "S0", "mu")
  out$input_range <- RR
  return(out)
}

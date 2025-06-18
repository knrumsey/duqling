#' Discrete Time Stochastic SIRS Model with Demography
#'
#' Dimensions: 9. This is a discrete time stochastic simulation of SIR with demography.
#' @param x Inputs of dimension (at least) 5. See below for details.
#' @param scale01 (Ignored here) When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE
#' @param Tf Number of time steps
#' @param N0 Initial population size
#' @return A multivariate time series giving S, I and R as a function of time step.
#' @details
#' Parameter description
#' \describe{
#'   \item{x1 = S0}{Proportion of population initally susceptible, [0, 1]}
#'   \item{x2 = I0}{Proportion of population initially infected, [0, 1]}
#'   \item{x3 = beta}{transimissability parameter, [0, 1]. The upper bound is not meaningful, and the number of new infections is given by `rbinom(1, S[t-1], 1 - exp(-beta*I[t-1]/N[t-1]))`.}
#'   \item{x4 = gamma}{recovery parameter, [0, 1]. The probability that each infectious person (independently) recovers at each time step. 1/gamma is the average length of infectious period}
#'   \item{x5 = alpha}{birth rate.  [0, 1]. Parameter range is not entirely meaningful. Births always enter into susceptible class as `rpois(1, alpha*N[t-1])`}
#'   \item{x6 = mu_s}{Death rate in susceptible class.  [0, 1]. Probability that a susceptible person dies during each time step.}
#'   \item{x7 = mu_i}{Death rate in infectious class.  [0, 1]. Probability that an infectious person dies during each time step.}
#'   \item{x8 = mu_r}{Death rate in recovered class.  [0, 1]. Probability that a recovered person dies during each time step.}
#'   \item{x9 = delta}{Re-susceptibility parameter.  [0, 1]. Probability that a recovered person becomes susceptible again during each time step.}
#'
#'
#' }
#' @references
#' add a reference
#' @export
#' @examples
#' x <- c(S0 = 0.99, I0 = 0.01,
#'    beta = 0.12, gamma = 0.1,
#'    alpha = 0, muS = 0, muI = 0,
#'    muR = 0, delta = 1/90)
#' set.seed(111)
#' sir <- dts_sirs(x, Tf = 365)
#' ts.plot(sir[,2], main="Number of infectious individuals", xlab="Time (days)", ylab="")
dts_sirs <- function(x, scale01=TRUE, Tf=90, N0= 1000){
  S <- I <- R <- N <- rep(0, Tf)
  N[1] <- N0
  S[1] <- round(x[1]*N0)
  I[1] <- round(x[2]*N0)
  R[1] <- N[1] - S[1] - I[1]

  for(t in 2:Tf){
    births <- rpois(1, x[5]*N[t-1])
    deaths <- rbinom(3, c(S[t-1], I[t-1], R[t-1]), x[6:8])
    infect <- rbinom(1, S[t-1]-deaths[1], 1 - exp(-x[3]*I[t-1]/N[t-1]))
    recove <- rbinom(1, I[t-1]-deaths[2], x[4])
    resusc <- rbinom(1, R[t-1]-deaths[3], x[9])

    S[t] <- S[t-1] + births - deaths[1] - infect + resusc
    I[t] <- I[t-1] - deaths[2] + infect - recove
    R[t] <- R[t-1] - deaths[3] + recove - resusc
    N[t] <- S[t] + I[t] + R[t]

    if(N[t] <= 0){
      out <- cbind(S, I, R)
      return(out)
    }
  }
  out <- cbind(S, I, R)
  return(out)
}


quackquack_dts_sirs <- function(){
  out <- list(input_dim=9)
  out$input_cat <- FALSE
  out$response_type <- c("func")

  RR <- cbind(rep(0, 9),
              rep(1, 9))
  rownames(RR) <- c("S0", "I0", "beta", "gamma", "alpha", "muS", "muI", "muR", "delta")
  out$input_range <- RR
  return(out)
}


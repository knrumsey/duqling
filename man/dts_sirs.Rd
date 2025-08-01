% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dts_sirs.R
\name{dts_sirs}
\alias{dts_sirs}
\title{Discrete Time Stochastic SIRS Model with Demography}
\usage{
dts_sirs(x, scale01 = TRUE, Tf = 90, N0 = 1000)
}
\arguments{
\item{x}{Inputs of dimension (at least) 5. See below for details.}

\item{scale01}{(Ignored here) When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE}

\item{Tf}{Number of time steps}

\item{N0}{Initial population size}
}
\value{
A multivariate time series giving S, I and R as a function of time step.
}
\description{
Dimensions: 9. This is a discrete time stochastic simulation of SIR with demography.
}
\details{
Parameter description
\describe{
  \item{x1 = I0}{Proportion of population initally infected, [0, 1]}
  \item{x2 = S0_without_I0}{Proportion of non-infected population initially susceptible, [0, 1]}
  \item{x3 = beta}{transimissability parameter, [0, 1]. The upper bound is not meaningful, and the number of new infections is given by `rbinom(1, S[t-1], 1 - exp(-beta*I[t-1]/N[t-1]))`.}
  \item{x4 = gamma}{recovery parameter, [0, 1]. The probability that each infectious person (independently) recovers at each time step. 1/gamma is the average length of infectious period}
  \item{x5 = alpha}{birth rate.  [0, 1]. Parameter range is not entirely meaningful. Births always enter into susceptible class as `rpois(1, alpha*N[t-1])`}
  \item{x6 = mu_s}{Death rate in susceptible class.  [0, 1]. Probability that a susceptible person dies during each time step.}
  \item{x7 = mu_i}{Death rate in infectious class.  [0, 1]. Probability that an infectious person dies during each time step.}
  \item{x8 = mu_r}{Death rate in recovered class.  [0, 1]. Probability that a recovered person dies during each time step.}
  \item{x9 = delta}{Re-susceptibility parameter.  [0, 1]. Probability that a recovered person becomes susceptible again during each time step.}


}
}
\examples{
x <- c(I0 = 0.01, S0_without_I0 = 1.00,
   beta = 0.12, gamma = 0.1,
   alpha = 0, muS = 0, muI = 0,
   muR = 0, delta = 1/90)
set.seed(111)
sir <- dts_sirs(x, Tf = 365)
ts.plot(sir[,2], main="Number of infectious individuals", xlab="Time (days)", ylab="")
}
\references{
add a reference
}

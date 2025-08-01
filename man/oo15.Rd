% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oakley_ohagan.R
\name{oo15}
\alias{oo15}
\title{Oakley and O'Hagan Function}
\usage{
oo15(x, scale01 = TRUE)
}
\arguments{
\item{x}{Inputs of dimension (at least) 15. See below for details.}

\item{scale01}{When TRUE, inputs are expected to be given on unit interval.}
}
\description{
Dimensions 15
}
\details{
This function's a-coefficients are chosen so that 5 of the input variables contribute significantly to the output variance, 5 have a much smaller effect, and the remaining 5 have almost no effect on the output variance. Values of the coefficient vectors a1, a2 and a3, and the matrix M, can be found at:
}
\examples{
n <- 50
p <- 15
X <- lhs::randomLHS(n, p)
y <- apply(X, 1, oo15, scale01=TRUE)
}
\references{
Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.

Oakley, J. E., & O'Hagan, A. (2004). Probabilistic sensitivity analysis of complex models: a Bayesian approach. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 66(3), 751-769.
}

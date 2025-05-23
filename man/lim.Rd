% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lim.R
\name{lim}
\alias{lim}
\alias{lim_polynomial}
\alias{lim_non_polynomial}
\title{Lim et al Test Functions}
\usage{
lim_polynomial(x, scale01 = TRUE)

lim_non_polynomial(x, scale01 = TRUE)
}
\arguments{
\item{x}{Inputs of dimension (at least) 8. See below for details.}

\item{scale01}{When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE}
}
\description{
Lim et al Test Functions
}
\details{
Two similar functions from Lim et al (2002). One is a polynomial the other is a non-polynomial but both functions have similar shape and behavior. For details, see \href{https://www.sfu.ca/~ssurjano/limetalpol.html}{the VLSE}.
}
\examples{
n <- 100 #Number of observations
p <- 6   #Number of variables
X <- matrix(runif(n*p), nrow=n)
y <- apply(X, 1, lim_polynomial)
}
\references{
Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.

Lim, Y. B., Sacks, J., Studden, W. J., & Welch, W. J. (2002). Design and analysis of computer experiments when the output is highly correlated over the input space. Canadian Journal of Statistics, 30(1), 109-126.
}

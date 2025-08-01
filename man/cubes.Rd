% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cubes.R
\name{cubes}
\alias{cubes}
\alias{cube3}
\title{Discontinuous Cube Functions}
\usage{
cube3(x, scale01 = TRUE)
}
\arguments{
\item{x}{Inputs of dimension (at least) 8. See below for details.}

\item{scale01}{(No effect here) When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE}
}
\description{
Discontinuous Cube Functions
}
\details{
Three cube functions
\enumerate{
   \item {cube3: a 3 dimensional test function equal to the sum of 3 indicator functions: f(x) = 1(0.2 < x1 < 0.5)1(0.1 < x2 < 0.6)1(x3 < 0.4) + 1(0.3 < x1 < 0.85)1(0.5 < x2 < 0.9)1(0.6 < x3) + 1(0.35 < x1 < 0.45)1(x2 < 0.75)}
   \item {cube3_rotate: equal to cube3(z1,z2,z3) after applying the rotation: 3z1 = x1 + x2 + x3, 3z2 = 1 + 2x1 - x2 + x3/10 , z3 = x3}
   \item {cube5: a 5 dimensional test function equal to cube3(x1,x2,x3) + prod(i=1:5)(0.25 < xi < 0.75) + 5*1(0.33 < x5)}
}
}
\examples{
n <- 100 #Number of observations
p <- 3   #Number of variables
X <- matrix(runif(n*p), nrow=n)
y <- apply(X, 1, cube3)
}

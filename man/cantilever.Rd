% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cantilever.R
\name{cantilever}
\alias{cantilever}
\alias{cantilever_D}
\alias{cantilever_S}
\title{The Cantilever Beam Function}
\usage{
cantilever_D(x, scale01 = TRUE, L = 100, D0 = 2.2535)

cantilever_S(x, scale01 = TRUE)
}
\arguments{
\item{x}{Inputs of dimension (at least) 5. See below for details.}

\item{scale01}{(Ignored here) When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE}

\item{L}{Length of beam. Default is 100 inches.}

\item{D0}{Displacement tolerance. Default is 2.2535 inches (Constraint: D < D_0).}
}
\value{
A scalar response
}
\description{
Dimensions: 6. The Cantilever Beam has two outputs: stress (S) and displacement (D). This can also be treated as a single function with bivariate output.
}
\details{
For details on the Cantilever Beam, see \href{https://www.sfu.ca/~ssurjano/canti.html}{the VLSE}.
}
\examples{
n <- 10
x <- matrix(runif(8*n), nrow=n)
y <- apply(x, 1, friedman, scale01=TRUE)
}
\references{
Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.

Sues, Robert, Mohammad Aminpour, and Youngwon Shin. "Reliability based MDO for aerospace systems." 19th AIAA Applied Aerodynamics Conference. 2000.

Wu, Y-T., et al. "Safety-factor based approach for probability-based design optimization." 19th AIAA applied aerodynamics conference. 2001.
}

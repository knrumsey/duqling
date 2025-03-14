% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grlee.R
\name{grlee}
\alias{grlee}
\alias{grlee6}
\alias{grlee1}
\alias{grlee2}
\title{Gramacy and Lee Test Functions}
\usage{
grlee6(x, scale01 = TRUE)

grlee1(x, scale01 = TRUE)

grlee2(x, scale01 = TRUE)
}
\arguments{
\item{x}{Inputs of dimension (at least) 8. See below for details.}

\item{scale01}{When TRUE, inputs are expected to be given on unit interval and are internally adjusted to their native range. Default is FALSE}
}
\description{
Gramacy and Lee Test Functions
}
\details{
Functions from three Gramacy and Lee papers. For details, see \href{https://www.sfu.ca/~ssurjano/emulat.html}{the VLSE} (\href{https://www.sfu.ca/~ssurjano/grlee12.html}{grlee1()}, \href{https://www.sfu.ca/~ssurjano/grlee08.html}{grlee2()}, \href{https://www.sfu.ca/~ssurjano/grlee09.html}{grlee6()}) .
}
\examples{
n <- 100 #Number of observations
p <- 6   #Number of variables
X <- matrix(runif(n*p), nrow=n)
y <- apply(X, 1, grlee6)
}
\references{
Surjanovic, Sonja, and Derek Bingham. "Virtual library of simulation experiments: test functions and datasets." Simon Fraser University, Burnaby, BC, Canada, accessed May 13 (2013): 2015.

Gramacy, R. B., & Lee, H. K. (2008). Gaussian processes and limiting linear models. Computational Statistics & Data Analysis, 53(1), 123-136.

Gramacy, R. B., & Lee, H. K. (2009). Adaptive design and analysis of supercomputer experiments. Technometrics, 51(2).

Gramacy, R. B., & Lee, H. K. (2012). Cases for the nugget in modeling computer experiments. Statistics and Computing, 22(3), 713-722.
}

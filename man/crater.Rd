% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crater.R
\name{crater}
\alias{crater}
\title{Crater Depth}
\usage{
crater(x, scale01 = TRUE, const = 0.0195, power = 0.45)
}
\arguments{
\item{x}{Inputs of dimension (at least) 7. See below for details.}

\item{scale01}{When TRUE, inputs are expected to be given on unit interval.}

\item{const}{default parameter}

\item{power}{default parameter}
}
\description{
Crater Depth
}
\details{
A physics-based function for predicting crater depth resulting from high-velocity impacts, such as those experienced by the Space Shuttle Columbia during reentry.
Input variables include length of projectile (in), diameter of projectile (in), density of projectile (lb/in^3), normal velocity (in/s), velocity required to break through tile coating (in/s), compressive tile strength (psi), and tile density (lb/in^3).
The \code{const} and \code{power} inputs default to 0.0195 and 0.45 per the "original R&C formula" but can be varied; Stellingwerf et al set these to 0.67 and 0.0086.
}
\examples{
X <- lhs::randomLHS(50, 7)
y <- apply(X, 1, crater, scale01=TRUE)
}
\references{
Stellingwerf, R., Robinson, J., Richardson, S., Evans, S., Stallworth, R., & Hovater, M. (2004). Foam-on-tile impact modeling for the STS-107 investigation. In 45th AIAA/ASME/ASCE/AHS/ASC Structures, Structural Dynamics & Materials Conference (p. 1881).
}

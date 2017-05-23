\name{NoiseGenerator}
\alias{NoiseGenerator}
\title{
Noise Generator Handle for Nonlinear Data Stream Generator (NLDSD)
}
\description{
Class which produces different types of noise for \code{NLDSD}.
}
\usage{
NoiseGenerator(noise.type, noise.parms, N)
}
\arguments{
  \item{noise.type}{Type of the noise, permitted values are \code{c("None", "Normal", "Uniform", "ARIMA")}.}
  \item{noise.parms}{Parameters in a list for the \code{noise.type}, which should be: for "Normal", \code{list(mean=0.0, sd=0.1)}; for "Uniform": \code{list(min=-0.1, max=0.1)}; and for "ARIMA": \code{order=1, ar=0.5, sd=0.1}.}
  \item{N}{The number of observations that should be created.}
}
\author{
    Fausto Guzzo da Costa
}
\seealso{
    \code{\link{DSCDD}} \code{\link{DSCDD_R}}
}

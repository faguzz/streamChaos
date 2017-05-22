\name{DSCDD}
\alias{DSCDD}
\alias{DSCDD_R}
\title{
Data Stream Concept Drift Detector
}
\description{
Abstract base class for all DSCDD (Data Stream Concept Drift Detectors) and DSCDD\_R classes.
}
\details{
The \code{DSCDD} and \code{DSCDD_R} classes cannot be instantiated, but they serve as a base class from which other DSCDD classes inherit.

Class \code{DSCDD} provides several generic functions: \code{print()}, \code{plot()}, \code{getMeasure()} to get the current measure based on input data, \code{reset()} to reset it to its initial state. The function \code{processStream()} process the whole stream, getting the measures after each interaction. \code{getMeasure()} and \code{reset()} are typically overwritten by subclasses of \code{DSCDD}. \code{DSCDD_R} provides these functions for R-based DSCDD implementations.
}
\author{
Fausto Guzzo da Costa
}
\seealso{
\code{\link{getMeasures}}
\code{\link[streamChaos]{plot}}
\code{\link{processStream}}
}

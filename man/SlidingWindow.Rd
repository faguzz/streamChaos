\name{SlidingWindow}
\alias{SlidingWindow}
\title{
Sliding Window Handle for Data Stream Concept Drift Detectors
}
\description{
Stores 
}
\usage{
SlidingWindow(window.length = 500, lambda = 1, compute.embedded.data = T, embedding.dimension = 2, delay.dimension = 1, compute.distances = F)
}
\arguments{
  \item{window.length}{The window length.}
  \item{compute.embedded.data}{If set to true, automatically computes the embedded data on every update.}
  \item{embedding.dimension}{The embedding dimension (used only if \code{compute.embedded.data=TRUE}.}
  \item{delay.dimension}{The delay dimension (used only if \code{compute.embedded.data=TRUE}.}
  \item{compute.distances}{If set to true, automatically computes the distance among points on every window update.}
}
\author{
    Fausto Guzzo da Costa
}
\seealso{
    \code{\link{DSCDD}} \code{\link{DSCDD_R}}
}

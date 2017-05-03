\name{NLDSD_LorenzAttractor}
\alias{NLDSD_LorenzAttractor}
\title{Attractor of Lorenz}
\description{
A data stream generator based on the three-dimensional Lorenz Attractor.}

\usage{
NLDSD_LorenzAttractor(N, start.x=c(5, 5, 5), parms=c(10, 28, 8/3), dt=0.1, warming.up.num=1000, range=NULL, noise.type="None", noise.parms=list())
}

\arguments{
  \item{N}{Determines the data stream length.}
  \item{start.x}{Determines the initial condition.}
  \item{parms}{The set of parameters omega, rho, and beta for the Lorenz' three-dimensional equation.}
  \item{dt}{}
  \item{warming.up.num}{Number of observations for warming up the attractor.}
  \item{range}{}
  \item{noise.type}{Type of the noise ("None", "Uniform", "Normal")} 
  \item{noise.parms}{A list for generating the noise.}
}

\details{
\code{NLDSD_LorenzAttractor} creates a data stream based on the three-dimensional Lorenz Attractor. Only the first dimension is returned.}

\value{
Returns a \code{NLDSD_LorenzAttractor} object (subclass of \code{NLDSD}) which is a list of the defined params.
}
	   
\seealso{
	\code{\link{NLDSD}}
}

\references{
}

\author{
    Fausto Guzzo da Costa
}


\examples{
# create data stream with three clusters in 3-dimensional data space
stream1 <- NLDSD_LorenzAttractor(N=10000)

plot(stream1)


# create data stream with specified clusterpositions, 
# 20% noise in a given bounding box and
# with different densities (1 to 9 between the two clusters) 
stream2 <- NLDSD_LorenzAttractor(N=10000, noise.type="Normal", noise.parms=list(mean=0, sd=0.1))
plot(stream2)
}

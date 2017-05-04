\name{NLDSD_TransientLogisticMap}
\alias{NLDSD_TransientLogisticMap}
\title{Transient Logistic Map}
\description{
A data stream generator based on the transient one-dimensional Logistic Map.
}

\usage{
NLDSD_TransientLogisticMap(N, start.x=c(0.6),
                           parm.range=list(t0=c(2.8), tn=c(4)),
                           dt=1, warming.up.num=0, range=NULL,
                           noise.type="None", noise.parms=list())
}

\arguments{
  \item{N}{Determines the data stream lenght.}
  \item{start.x}{Determines the initial condition.}
  \item{parm.range}{A list, of the parameter \eqn{r} at the beginning and at the end of the data generator.}
  \item{dt}{}
  \item{warming.up.num}{Number of observations for warming up the attractor.}
  \item{range}{}
  \item{noise.type}{Type of the noise ("None", "Uniform", "Normal")}
  \item{noise.parms}{A list for generating the noise.}
}

\details{
\code{NLDSD_TransientLogisticMap} creates a transient data stream based on the one-dimensional Logistic Map:
\deqn{x_{t+1} = r_t x_t (1 - x_t),}
in which the parameter \eqn{r_t} is changed between \eqn{parm.range[t0]} and \eqn{parm.range[tn]}, producing a data stream which dynamics change over time.
}

\value{
Returns a \code{NLDSD_TransientLogisticMap} object (subclass of \code{NLDSD}) which is a list of the defined params.
}

\seealso{
\code{\link{NLDSD}}, \code{\link{NLDSD_LogisticMap}}
}

\references{
}

\author{
Fausto Guzzo da Costa
}

\examples{
# create a data stream with 10,000 observations based on a
# Transient Logistic Map, and plot it
stream1 <- NLDSD_TransientLogisticMap(N=10000)
plot(stream1)


# create a data stream with 10,000 observations based on a
# Transient Logistic Map, with noise produced by a Normal
# distribution with mean=0 and std=0.1 and plot it
stream2 <- NLDSD_TransientLogisticMap(N=10000, noise.type="Normal",
                                      noise.parms=list(mean=0, sd=0.1))
plot(stream2)
}

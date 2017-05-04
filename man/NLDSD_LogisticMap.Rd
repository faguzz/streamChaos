\name{NLDSD_LogisticMap}
\alias{NLDSD_LogisticMap}
\title{Logistic Map}
\description{
A data stream generator based on the one-dimensional Logistic Map.
}

\usage{
NLDSD_LogisticMap(N, start.x = c(0.6), parms = c(3.8),
                  dt = 1, warming.up.num = 1000,
                  range = NULL, noise.type = "None",
                  noise.parms = list())
}

\arguments{
  \item{N}{Determines the data stream lenght.}
  \item{start.x}{Determines the initial condition.}
  \item{parms}{The parameter \eqn{r}.}
  \item{dt}{}
  \item{warming.up.num}{Number of observations for warming up the attractor.}
  \item{range}{}
  \item{noise.type}{Type of the noise ("None", "Uniform", "Normal")}
  \item{noise.parms}{A list for generating the noise.}
}

\details{
\code{NLDSD_LogisticMap} creates a data stream based on the one-dimensional Logistic Map:
\deqn{x_{t+1} = r x_t (1 - x_t),}
in which \eqn{r} is a parameter commonly set between \eqn{2.0} and \eqn{4.0}. Depending on such value, the data produced may be regular or chaotic.
}

\value{
Returns a \code{NLDSD_LogisticMap} object (subclass of \code{NLDSD}) which is a list of the defined params.
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
# create a data stream with 10,000 observations based on a Logistic Map
# and plot it
stream1 <- NLDSD_LogisticMap(N=10000)
plot(stream1)


# create a data stream with 10,000 observations based on a Logistic Map
# with noise produced by a Normal distribution
# with mean=0 and std=0.1
# and plot it
stream2 <- NLDSD_LogisticMap(N=10000, noise.type="Normal",
                             noise.parms=list(mean=0, sd=0.1))
plot(stream2)
}

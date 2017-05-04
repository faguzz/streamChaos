\name{NLDSD_LorenzAttractor}
\alias{NLDSD_LorenzAttractor}
\title{Attractor of Lorenz}
\description{
A data stream generator based on the three-dimensional Lorenz Attractor.
}

\usage{
NLDSD_LorenzAttractor(N, start.x=c(5, 5, 5), parms=c(10, 28, 8/3),
                      dt=0.1, warming.up.num=1000, range=NULL,
                      noise.type="None", noise.parms=list())
}

\arguments{
  \item{N}{Determines the data stream length.}
  \item{start.x}{Determines the initial condition.}
  \item{parms}{The set of parameters \eqn{\sigma}, \eqn{\rho}, and \eqn{\beta} for the Lorenz' three-dimensional equation.}
  \item{dt}{}
  \item{warming.up.num}{Number of observations for warming up the attractor.}
  \item{range}{}
  \item{noise.type}{Type of the noise ("None", "Uniform", "Normal")} 
  \item{noise.parms}{A list for generating the noise.}
}

\details{
\code{NLDSD_LorenzAttractor} creates a data stream based on the three-dimensional Lorenz Attractor:

\deqn{
\begin{array}{ll}
    x_{t+1} = \sigma (y - x_t), \\
    y_{t+1} = x_t (\rho - z_t) - y_t, \\
    z_{t+1} = x_t y_t - \beta z_t,
\end{array}
}{
x_{t+1} = \sigma (y - x_t),
y_{t+1} = x_t (\rho - z_t) - y_t,
z_{t+1} = x_t y_t - \beta z_t,
}

in which the parameters \eqn{\sigma}, \eqn{\rho}, and \eqn{\beta} change the data behavior. Traditionally, they are set to \eqn{10}, \eqn{28}, and \eqn{8/3} respectively, producing a chaotic data. Other values may be used, as shown in Wikipedia.

In this code, only the variable \eqn{x_t} is returned.
}

\value{
Returns a \code{NLDSD_LorenzAttractor} object (subclass of \code{NLDSD}) which is a list of the defined params.
}
	   
\seealso{
\code{\link{NLDSD}}
}

\references{
https://en.wikipedia.org/wiki/Lorenz_system
}

\author{
Fausto Guzzo da Costa
}


\examples{
# create a data stream with 10,000 observations based on a Logistic Map
# and plot it
stream1 <- NLDSD_LorenzAttractor(N=10000)
plot(stream1)


# create a data stream with 10,000 observations based on a Logistic Map
# with noise produced by a Normal distribution
# with mean=0 and std=0.1
# and plot it
stream2 <- NLDSD_LorenzAttractor(N=10000, noise.type="Normal",
                                 noise.parms=list(mean=0, sd=0.1))
plot(stream2)
}

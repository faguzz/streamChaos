\name{NLDSD_RosslerAttractor}
\alias{NLDSD_RosslerAttractor}
\title{Attractor of Rössler}
\description{
A data stream generator based on the three-dimensional Rössler Attractor.
}
\usage{
NLDSD_RosslerAttractor(N, start.x=c(0, 0, 0), parms=c(0.15, 0.2, 10),
                       dt=0.3, warming.up.num=1000, range=NULL,
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
    x_{t+1} = -y_t - z_t, \\
    y_{t+1} = x_t + a y_t, \\
    z_{t+1} = b + z_t (x_t + c),
\end{array}
}{
x_{t+1} = -y_t - z_t,
y_{t+1} = x_t + a y_t,
z_{t+1} = b + z_t (x_t + c),
}

in which the parameters \eqn{a}, \eqn{b}, and \eqn{c} change the data behavior. Traditionally, they are set to \eqn{0.15}, \eqn{0.2}, and \eqn{10} respectively, producing a chaotic data. Other values may be used, as shown in Wikipedia.

In this code, only the variable \eqn{x_t} is returned.
}

\value{
Returns a \code{NLDSD_RosslerAttractor} object (subclass of \code{NLDSD}) which is a list of the defined params.
}

\seealso{
\code{\link{NLDSD}}
}

\references{
https://en.wikipedia.org/wiki/Rössler_attractor
}

\author{
Fausto Guzzo da Costa
}


\examples{
# create a data stream with 10,000 observations based on a Rossler Attractor
# and plot it
stream1 <- NLDSD_RosslerAttractor(N=10000)
plot(stream1)


# create a data stream with 10,000 observations based on a Rossler Attractor
# with noise produced by a Normal distribution
# with mean=0 and std=0.1
# and plot it
stream2 <- NLDSD_RosslerAttractor(N=10000, noise.type="Normal",
                                  noise.parms=list(mean=0, sd=0.1))
plot(stream2)
}

\name{NoiseGenerator}
\alias{NoiseGenerator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NoiseGenerator(noise.type, noise.parms, N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{noise.type}{
%%     ~~Describe \code{noise.type} here~~
}
  \item{noise.parms}{
%%     ~~Describe \code{noise.parms} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (noise.type, noise.parms, N) 
{
    if (!is.null(noise.parms$seed)) {
        set.seed(noise.parms$seed)
    }
    noise <- switch(noise.type, None = rep(0, N), Normal = rnorm(N, 
        noise.parms$mean, noise.parms$sd), Uniform = runif(N, 
        min = noise.parms$min, max = noise.parms$max), ARIMA = arima.sim(list(order = noise.parms$order, 
        ar = noise.parms$ar), sd = noise.parms$sd, n = N))
    noise
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

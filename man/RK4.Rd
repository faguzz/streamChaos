\name{RK4}
\alias{RK4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RK4(t, syst, y, dt, parms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{
%%     ~~Describe \code{t} here~~
}
  \item{syst}{
%%     ~~Describe \code{syst} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{dt}{
%%     ~~Describe \code{dt} here~~
}
  \item{parms}{
%%     ~~Describe \code{parms} here~~
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
function (t, syst, y, dt, parms) 
{
    k1 <- syst(t, y, parms)[[1]] * dt
    k2 <- syst(t + dt/2, y + dt/2 * k1, parms)[[1]] * dt
    k3 <- syst(t + dt/2, y + dt/2 * k2, parms)[[1]] * dt
    k4 <- syst(t + dt, y + dt * k3, parms)[[1]] * dt
    dy <- y + dt/6 * (k1 + 2 * k2 + 2 * k3 + k4)
    list(c(dy))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{MapIterator}
\alias{MapIterator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MapIterator(syst, start.time, end.time, dt, start.x, parms = NULL, runge.kutta = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{syst}{
%%     ~~Describe \code{syst} here~~
}
  \item{start.time}{
%%     ~~Describe \code{start.time} here~~
}
  \item{end.time}{
%%     ~~Describe \code{end.time} here~~
}
  \item{dt}{
%%     ~~Describe \code{dt} here~~
}
  \item{start.x}{
%%     ~~Describe \code{start.x} here~~
}
  \item{parms}{
%%     ~~Describe \code{parms} here~~
}
  \item{runge.kutta}{
%%     ~~Describe \code{runge.kutta} here~~
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
function (syst, start.time, end.time, dt, start.x, parms = NULL, 
    runge.kutta = F) 
{
    times <- seq(start.time, end.time, by = dt)
    y <- start.x
    ret <- NULL
    for (t in times) {
        ret <- rbind(ret, y)
        if (runge.kutta) {
            y <- RK4(t, syst, y, dt, parms)[[1]]
        }
        else {
            y <- syst(t, y, parms)[[1]] * dt
        }
    }
    ret
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

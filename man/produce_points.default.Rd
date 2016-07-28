\name{produce_points.default}
\alias{produce_points.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
produce_points.default(dsd, n = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsd}{
%%     ~~Describe \code{dsd} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
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
function (dsd, n = 1) 
{
    n <- as.integer(n)
    if (!dsd$is.transient) {
        obs <- MapIterator(dsd$syst, 0, n * dsd$dt, dsd$dt, dsd$state$start.x, 
            dsd$state$parms, runge.kutta = dsd$runge.kutta)
    }
    else {
        obs <- TransientMapIterator(dsd$syst, 0, n * dsd$dt, 
            dsd$dt, dsd$state$start.x, dsd$state$parms, dsd$parms.mod, 
            runge.kutta = dsd$runge.kutta)
    }
    last_obs <- tail(obs, n = 1)
    current_obs <- head(obs, n = n)
    data <- data.frame(X1 = current_obs[, 1])
    data <- NormalizeData(data, dsd$range)
    time <- seq(dsd$state$counter, dsd$state$counter + (n - 1) * 
        dsd$dt, by = dsd$dt)
    rownames(data) <- time
    dsd$state$counter <- dsd$state$counter + n * dsd$dt
    dsd$state$start.x <- last_obs
    if (dsd$is.transient) {
        dsd$state$parms <- dsd$state$parms + dsd$parms.mod * 
            n
    }
    data
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

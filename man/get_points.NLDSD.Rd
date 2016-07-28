\name{get_points.NLDSD}
\alias{get_points.NLDSD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_points.NLDSD(dsd, n = 1, outofpoints = c("stop", "warn", "ignore"), assignment = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsd}{
%%     ~~Describe \code{dsd} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{outofpoints}{
%%     ~~Describe \code{outofpoints} here~~
}
  \item{assignment}{
%%     ~~Describe \code{assignment} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (dsd, n = 1, outofpoints = c("stop", "warn", "ignore"), 
    assignment = F, ...) 
{
    n <- as.integer(n)
    outofpoints <- match.arg(outofpoints)
    if (!dsd$state$warmed.up) {
        warm_up(dsd)
        dsd$state$warmed.up <- T
    }
    if (dsd$state$counter >= (dsd$N * dsd$dt)) {
        if (outofpoints == "stop") 
            stop("The stream is at its end!")
        if (outofpoints == "warn") 
            warning("The stream is at its end! No more points available!")
    }
    if ((dsd$state$counter + n * dsd$dt) > (dsd$N * dsd$dt)) {
        n <- ((dsd$N + 1) * dsd$dt - dsd$state$counter)/dsd$dt
    }
    data <- produce_points(dsd, n)
    data <- add_noise(dsd, data)
    if (assignment) {
        time <- as.numeric(rownames(data))
        assigments <- rep(0, n)
        if (!is.null(dsd$time.changes)) {
            change.times <- intersect(time, as.integer(dsd$time.changes))
            assigments[change.times - head(time, n = 1)] <- 1
        }
        data <- cbind(data, assigments)
    }
    data
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

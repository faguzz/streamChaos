\name{processStream}
\alias{processStream}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
processStream(dsd, alg, N, window.step = 10, plot = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsd}{
%%     ~~Describe \code{dsd} here~~
}
  \item{alg}{
%%     ~~Describe \code{alg} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{window.step}{
%%     ~~Describe \code{window.step} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
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
function (dsd, alg, N, window.step = 10, plot = F) 
{
    ds <- NULL
    measures <- c()
    begin.time <- Sys.time()
    while (T) {
        ret <- tryCatch(p <- get_points(dsd, n = window.step, 
            outofpoints = "warn", assignment = T), warning = function(w) {
            w
        })
        if ("warning" \%in\% class(ret)) {
            break
        }
        ds <- rbind(ds, p)
        update(alg, p)
        val <- getMeasure(alg)
        measures <- c(measures, val)
    }
    if (plot) {
        plot(ds[, 1], pch = ".")
        par(new = T)
        times <- seq(window.step, nrow(ds), by = window.step)
        plot(times, measures, type = "l", lwd = 2, col = 2, axes = F)
        abline(v = which(ds[, 2] == 1))
        axis(4, at = pretty(measures))
    }
    ret = list()
    ret$ds <- ds
    ret$measures <- measures
    ret$total.time <- as.numeric(Sys.time() - begin.time, units = "secs")
    ret
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

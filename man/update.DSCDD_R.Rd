\name{update.DSCDD_R}
\alias{update.DSCDD_R}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
update.DSCDD_R(object, dsd, n = 1, verbose = FALSE, block = 10000L, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{dsd}{
%%     ~~Describe \code{dsd} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{block}{
%%     ~~Describe \code{block} here~~
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
function (object, dsd, n = 1, verbose = FALSE, block = 10000L, 
    ...) 
{
    if (is.data.frame(dsd) || is.matrix(dsd)) {
        if (verbose) 
            cat("Clustering all data at once for matrix/data.frame.")
        object$RObj$process(dsd, ...)
        return(invisible(object))
    }
    n <- as.integer(n)
    block <- as.integer(block)
    if (n > 0) {
        if (!is(dsd, "DSD_data.frame")) 
            stop("Cannot cluster stream (need a DSD_data.frame.)")
        if (verbose) 
            total <- 0L
        for (bl in .make_block(n, block)) {
            object$RObj$process(get_points(dsd, bl), ...)
            if (verbose) {
                total <- total + bl
                cat("Processed", total, "points\n")
            }
        }
    }
    invisible(object)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

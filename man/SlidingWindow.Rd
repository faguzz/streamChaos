\name{SlidingWindow}
\alias{SlidingWindow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SlidingWindow(window.length = 500, lambda = 1, compute.embedded.data = T, embedding.dimension = 2, delay.dimension = 1, compute.distances = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{window.length}{
%%     ~~Describe \code{window.length} here~~
}
  \item{lambda}{
%%     ~~Describe \code{lambda} here~~
}
  \item{compute.embedded.data}{
%%     ~~Describe \code{compute.embedded.data} here~~
}
  \item{embedding.dimension}{
%%     ~~Describe \code{embedding.dimension} here~~
}
  \item{delay.dimension}{
%%     ~~Describe \code{delay.dimension} here~~
}
  \item{compute.distances}{
%%     ~~Describe \code{compute.distances} here~~
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
function (window.length = 500, lambda = 1, compute.embedded.data = T, 
    embedding.dimension = 2, delay.dimension = 1, compute.distances = F) 
{
    if (window.length < 1) {
        stop("Window too small")
    }
    if (lambda > 1 || lambda < 0) {
        stop("Lambda not acceptable")
    }
    if (compute.embedded.data == T && embedding.dimension < 2) {
        stop("Cannot embedd data with this embedding dimension")
    }
    if (compute.embedded.data == T && delay.dimension < 1) {
        stop("Cannot embedd data with this time delay")
    }
    if (compute.distances == T && compute.embedded.data == F) {
        stop("Cannot compute distances without computing embedded data")
    }
    max.length <- window.length
    lambda <- lambda
    m <- embedding.dimension
    d <- delay.dimension
    compute.embedded.data <- compute.embedded.data
    compute.distances <- compute.distances
    embedded.data <- NULL
    count <- 0
    data <- c()
    l <- list(description = "Sliding Window", max.length = max.length, 
        lambda = lambda, m = m, d = d, compute.embedded.data = compute.embedded.data, 
        compute.distances = compute.distances, count = count, 
        data = data, embedded.data = embedded.data)
    class(l) <- c("SlidingWindow")
    l
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

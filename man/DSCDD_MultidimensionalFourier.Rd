\name{DSCDD_MultidimensionalFourier}
\alias{DSCDD_MultidimensionalFourier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DSCDD_MultidimensionalFourier(m = 2, d = 1, nbins = 7, window.length = 800)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
%%     ~~Describe \code{m} here~~
}
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{nbins}{
%%     ~~Describe \code{nbins} here~~
}
  \item{window.length}{
%%     ~~Describe \code{window.length} here~~
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
function (m = 2, d = 1, nbins = 7, window.length = 800) 
{
    multidimensionalFourier <- multidimensionalFourier$new(m = m, 
        d = d, nbins = nbins, window.length = window.length)
    description <- "Multidimensional Fourier"
    l <- list(description = description, RObj = multidimensionalFourier)
    class(l) <- c("DSCDD_MultidimensionalFourier", "DSCDD_R", 
        "DSCDD")
    l
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{DSCDD_RQA}
\alias{DSCDD_RQA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DSCDD_RQA(m = 2, d = 1, window.length = 800, radius, measure.type = c("RR", "DET", "LMAX"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
%%     ~~Describe \code{m} here~~
}
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{window.length}{
%%     ~~Describe \code{window.length} here~~
}
  \item{radius}{
%%     ~~Describe \code{radius} here~~
}
  \item{measure.type}{
%%     ~~Describe \code{measure.type} here~~
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
function (m = 2, d = 1, window.length = 800, radius, measure.type = c("RR", 
    "DET", "LMAX")) 
{
    rqa <- rqa$new(m = m, d = d, radius = radius, window.length = window.length, 
        measure.type = measure.type)
    description <- "Recurrence Quantification Analysis"
    l <- list(description = description, RObj = rqa)
    class(l) <- c("DSCDD_RQA", "DSCDD_R", "DSCDD")
    l
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{DSCDD_GromovHausdorff}
\alias{DSCDD_GromovHausdorff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DSCDD_GromovHausdorff(m = 2, d = 1, window.length = 800, landmarking = c("none", "random", "kmeans"), landmarks.num = window.length/10)
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
  \item{landmarking}{
%%     ~~Describe \code{landmarking} here~~
}
  \item{landmarks.num}{
%%     ~~Describe \code{landmarks.num} here~~
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
function (m = 2, d = 1, window.length = 800, landmarking = c("none", 
    "random", "kmeans"), landmarks.num = window.length/10) 
{
    gromovHausdorff <- gromovHausdorff$new(m = m, d = d, window.length = window.length, 
        landmarking = landmarking, landmarks.num = landmarks.num)
    description <- "Gromov-Hausdorff distance"
    l <- list(description = description, RObj = gromovHausdorff)
    class(l) <- c("DSCDD_GromovHausdorff", "DSCDD_R", "DSCDD")
    l
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

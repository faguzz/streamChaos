\name{NLDSD_LogisticMap}
\alias{NLDSD_LogisticMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NLDSD_LogisticMap(N, start.x = c(0.6), parms = c(3.8), dt = 1, warming.up.num = 1000, range = NULL, noise.type = "None", noise.parms = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{start.x}{
%%     ~~Describe \code{start.x} here~~
}
  \item{parms}{
%%     ~~Describe \code{parms} here~~
}
  \item{dt}{
%%     ~~Describe \code{dt} here~~
}
  \item{warming.up.num}{
%%     ~~Describe \code{warming.up.num} here~~
}
  \item{range}{
%%     ~~Describe \code{range} here~~
}
  \item{noise.type}{
%%     ~~Describe \code{noise.type} here~~
}
  \item{noise.parms}{
%%     ~~Describe \code{noise.parms} here~~
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
function (N, start.x = c(0.6), parms = c(3.8), dt = 1, warming.up.num = 1000, 
    range = NULL, noise.type = "None", noise.parms = list()) 
{
    if (is.null(range)) {
        range <- matrix(c(0, 1), ncol = 2, byrow = T)
    }
    if (N < 1) 
        stop("invalid number of observations")
    counter <- 0
    warmed.up <- F
    state <- new.env()
    assign("counter", counter, envir = state)
    assign("start.x", start.x, envir = state)
    assign("parms", parms, envir = state)
    assign("warmed.up", warmed.up, envir = state)
    l <- list(description = "Logistic Map Stream", N = N, range = range, 
        dt = dt, syst = LogisticMap, is.transient = F, runge.kutta = F, 
        warming.up.num = warming.up.num, noise.type = noise.type, 
        noise.parms = noise.parms, state = state)
    class(l) <- c("NLDSD_LogisticMap", "NLDSD", "DSD_R", "DSD_data.frame", 
        "DSD")
    l
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

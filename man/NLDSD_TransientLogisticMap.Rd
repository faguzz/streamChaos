\name{NLDSD_TransientLogisticMap}
\alias{NLDSD_TransientLogisticMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NLDSD_TransientLogisticMap(N, start.x = c(0.6), parm.range = list(t0 = c(2.8), tn = c(4)), dt = 1, warming.up.num = 0, range = NULL, noise.type = "None", noise.parms = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{start.x}{
%%     ~~Describe \code{start.x} here~~
}
  \item{parm.range}{
%%     ~~Describe \code{parm.range} here~~
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
function (N, start.x = c(0.6), parm.range = list(t0 = c(2.8), 
    tn = c(4)), dt = 1, warming.up.num = 0, range = NULL, noise.type = "None", 
    noise.parms = list()) 
{
    if (is.null(range)) {
        range <- matrix(c(0, 1), ncol = 2, byrow = T)
    }
    if (N < 1) 
        stop("invalid number of observations")
    counter <- 0
    warmed.up <- F
    parms <- parm.range$t0
    parms.mod <- (parm.range$tn - parm.range$t0)/N
    state <- new.env()
    assign("counter", counter, envir = state)
    assign("start.x", start.x, envir = state)
    assign("parms", parms, envir = state)
    assign("warmed.up", warmed.up, envir = state)
    change.times <- c(3.022, 3.464, 3.554, 3.571, 3.628, 3.635, 
        3.739, 3.744, 3.829, 3.85)
    time.changes <- c((change.times - parms)/parms.mod)
    l <- list(description = "Transient Logistic Map Stream", 
        N = N, range = range, dt = dt, syst = LogisticMap, is.transient = T, 
        runge.kutta = F, warming.up.num = warming.up.num, noise.type = noise.type, 
        noise.parms = noise.parms, parms.mod = parms.mod, time.changes = time.changes, 
        state = state)
    class(l) <- c("NLDSD_TransientLogisticMap", "NLDSD", "DSD_R", 
        "DSD_data.frame", "DSD")
    l
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

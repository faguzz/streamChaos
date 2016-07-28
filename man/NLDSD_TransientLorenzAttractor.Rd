\name{NLDSD_TransientLorenzAttractor}
\alias{NLDSD_TransientLorenzAttractor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NLDSD_TransientLorenzAttractor(N, start.x = c(5, 5, 5), parm.range = list(t0 = c(10, 28, 8/3), tn = c(10, 268, 8/3)), dt = 0.1, warming.up.num = 0, range = NULL, noise.type = "None", noise.parms = list())
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
function (N, start.x = c(5, 5, 5), parm.range = list(t0 = c(10, 
    28, 8/3), tn = c(10, 268, 8/3)), dt = 0.1, warming.up.num = 0, 
    range = NULL, noise.type = "None", noise.parms = list()) 
{
    if (is.null(range)) {
        range <- matrix(c(-54, 56), ncol = 2, byrow = T)
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
    change.times <- c(100, 145, 166, 214.4)
    time.changes <- c((change.times - parms[2])/parms.mod[2]) * 
        dt
    l <- list(description = "Transient Lorenz Attractor Stream", 
        N = N, range = range, dt = dt, syst = LorenzAttractor, 
        is.transient = T, runge.kutta = T, warming.up.num = warming.up.num, 
        noise.type = noise.type, noise.parms = noise.parms, parms.mod = parms.mod, 
        time.changes = time.changes, state = state)
    class(l) <- c("NLDSD_TransientLorenzAttractor", "NLDSD", 
        "DSD_R", "DSD_data.frame", "DSD")
    l
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

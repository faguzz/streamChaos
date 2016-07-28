\name{update.SlidingWindow}
\alias{update.SlidingWindow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
update.SlidingWindow(window, new.data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{window}{
%%     ~~Describe \code{window} here~~
}
  \item{new.data}{
%%     ~~Describe \code{new.data} here~~
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
function (window, new.data) 
{
    num.itens <- length(new.data)
    if (num.itens > window$max.length) 
        stop("Updated more points than the size of the window")
    excess.data <- window$count + num.itens - window$max.length
    if (excess.data > 0) {
        window$data <- window$data[-1:-excess.data]
    }
    window$data <- c(window$data, new.data)
    window$count <- length(window$data)
    if (window$compute.embedded.data) {
        if (is.null(window$embedded.data)) {
            if (window$count >= (window$m - 1) * window$d + 1) {
                window$embedded.data <- embedd(window$data, window$m, 
                  window$d)
                if (window$compute.distances) {
                  window$distances <- as.matrix(dist(window$embedded.data))
                }
            }
        }
        else {
            if (excess.data > 0) {
                window$embedded.data <- window$embedded.data[-1:-excess.data, 
                  ]
            }
            range.embbed.data <- c((window$count - num.itens - 
                ((window$m - 1) * window$d) + 1), window$count)
            if (range.embbed.data[1] < 1) {
                range.embbed.data[1] <- 1
            }
            data.to.embedd <- window$data[range.embbed.data[1]:range.embbed.data[2]]
            new.embedded.data <- embedd(data.to.embedd, window$m, 
                window$d)
            if (window$compute.distances) {
                distance.between.points <- proxy::dist(window$embedded.dat, 
                  new.embedded.data)
            }
            window$embedded.data <- rbind(window$embedded.data, 
                new.embedded.data)
            if (window$compute.distances) {
                if (excess.data > 0) {
                  old.distances <- window$distances[-1:-excess.data, 
                    -1:-excess.data]
                }
                else {
                  old.distances <- window$distances
                }
                new.distances <- as.matrix(dist(new.embedded.data))
                window$distances <- rbind(cbind(old.distances, 
                  distance.between.points), cbind(t(distance.between.points), 
                  new.distances))
            }
        }
    }
    window
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{multidimensionalFourier-class}
\Rdversion{1.1}
\docType{class}
\alias{multidimensionalFourier-class}
\alias{multidimensionalFourier}

\title{Class \code{"multidimensionalFourier"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("multidimensionalFourier")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{m}:}{Object of class \code{numeric} ~~ }
    \item{\code{d}:}{Object of class \code{numeric} ~~ }
    \item{\code{nbins}:}{Object of class \code{numeric} ~~ }
    \item{\code{window.length}:}{Object of class \code{numeric} ~~ }
    \item{\code{window.data}:}{Object of class \code{ANY} ~~ }
    \item{\code{coordinates}:}{Object of class \code{ANY} ~~ }
    \item{\code{modelA}:}{Object of class \code{ANY} ~~ }
    \item{\code{modelB}:}{Object of class \code{ANY} ~~ }
    \item{\code{verbose}:}{Object of class \code{logical} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{reset()}:}{ ~~ }
    \item{\code{printParameters()}:}{ ~~ }
    \item{\code{multidimensionalFourierTransform(grid)}:}{ ~~ }
    \item{\code{assimilateToGrid(grid, key)}:}{ ~~ }
    \item{\code{multidimensionalQuantize(data)}:}{ ~~ }
    \item{\code{getMeasure(...)}:}{ ~~ }
    \item{\code{process(newdata, ...)}:}{ ~~ }
    \item{\code{initialize(m, d, nbins, window.length)}:}{ ~~ }
  }
}

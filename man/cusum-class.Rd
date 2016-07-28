\name{cusum-class}
\Rdversion{1.1}
\docType{class}
\alias{cusum-class}
\alias{cusum}

\title{Class \code{"cusum"}}
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
showClass("cusum")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{h}:}{Object of class \code{numeric} ~~ }
    \item{\code{delta}:}{Object of class \code{numeric} ~~ }
    \item{\code{n}:}{Object of class \code{numeric} ~~ }
    \item{\code{m}:}{Object of class \code{numeric} ~~ }
    \item{\code{variance}:}{Object of class \code{numeric} ~~ }
    \item{\code{M2}:}{Object of class \code{numeric} ~~ }
    \item{\code{Si}:}{Object of class \code{numeric} ~~ }
    \item{\code{Gi}:}{Object of class \code{numeric} ~~ }
    \item{\code{Sd}:}{Object of class \code{numeric} ~~ }
    \item{\code{Gd}:}{Object of class \code{numeric} ~~ }
    \item{\code{verbose}:}{Object of class \code{logical} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{reset()}:}{ ~~ }
    \item{\code{printParameters()}:}{ ~~ }
    \item{\code{getMeasure(...)}:}{ ~~ }
    \item{\code{process(newdata, ...)}:}{ ~~ }
    \item{\code{initialize(h, delta)}:}{ ~~ }
  }
}

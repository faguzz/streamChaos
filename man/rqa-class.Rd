\name{rqa-class}
\Rdversion{1.1}
\docType{class}
\alias{rqa-class}
\alias{rqa}

\title{Class \code{"rqa"}}
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
showClass("rqa")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{m}:}{Object of class \code{numeric} ~~ }
    \item{\code{d}:}{Object of class \code{numeric} ~~ }
    \item{\code{radius}:}{Object of class \code{numeric} ~~ }
    \item{\code{measure.type}:}{Object of class \code{character} ~~ }
    \item{\code{window.length}:}{Object of class \code{numeric} ~~ }
    \item{\code{window.data}:}{Object of class \code{ANY} ~~ }
    \item{\code{results.file}:}{Object of class \code{character} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{reset()}:}{ ~~ }
    \item{\code{printParameters()}:}{ ~~ }
    \item{\code{computeR()}:}{ ~~ }
    \item{\code{computeRQA(...)}:}{ ~~ }
    \item{\code{getMeasure(...)}:}{ ~~ }
    \item{\code{process(newdata, ...)}:}{ ~~ }
    \item{\code{initialize(m, d, radius, measure.type, window.length)}:}{ ~~ }
  }
}

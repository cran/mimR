\name{toMIM}
\alias{toMIM}
\alias{toMIM.data.frame}
\alias{toMIM.gmData}
\alias{toMIM.table}
\alias{toMIM.momentstats}




%- Also NEED an '\alias' for EACH other topic documented here.
\title{Wrapper for submitting data and models to MIM}
\description{
  Function for submitting data and models to MIM. The function is
  primarily intended for internal use
}
\usage{
toMIM(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Data can be either 1) a dataframe, 2) a table, 3) an
    internal structure called momentstats or 4) a gmData object }
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  NULL
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Second Edition, Springer Verlag, 2000}
\author{S�ren H�jsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

% ~Make other sections like Warning with \section{Warning }{....} ~ 
%\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
%\examples{
%}
\keyword{models}

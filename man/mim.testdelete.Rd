\name{mim.testdelete}
\alias{mim.testdelete}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Function to test deletion of an edge }
\description{
Function to test deletion of an edge 
}
\usage{
mim.testdelete(edge, options = NULL)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{edge}{The edge given as a string}
  \item{options}{The options which can be given to the command
    testdelete in MIM}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  A string with the result of the test
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}
% ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{\code{\link{mim.stepwise}}}

\examples{
data(carcass)
mim.read(carcass, mim.names=c("a","b","c","d","e","f","y"))

mim.cmd("model //abcdefy")
mim.testdelete("ay")
mim.testdelete("ay",options="S")
}
\keyword{models}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{mim.stepwise}
\alias{mim.stepwise}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Function to do stepwise model selection in MIM}
\description{
  Function to do stepwise model selection in MIM
}
\usage{
mim.stepwise(options = NULL, short = FALSE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{options}{The options are those letters which can be given to the
  stepwise command in MIM}
  \item{short}{if TRUE only a short summary of the result of the model
    selection is printed.}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  A string containing the finally selected model
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}
% ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{\code{\link{mim.testdelete}}}

\examples{
data(carcass)
mim.read(carcass, mim.names=c("a","b","c","d","e","f","y"))

mim.cmd("model //abcdefy")
mim.stepwise()
}
%\keyword{utilities}
\keyword{models}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line


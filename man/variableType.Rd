\name{variableType}
\alias{variableType}
\alias{is.discrete}
\alias{is.continuous}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Information about variables in MIM model}
\description{
Retrieve information about variables in MIM model
}
\usage{
variableType(object)
%is.discrete(mim)
%is.continuous(mim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A mim model object}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  variableType() returns either "discrete", "continuous" or "mixed".
  is.discrete() and is.continuous() return TRUE or FALSE

}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Second Edition, Springer Verlag, 2000}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}


%\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
}
\keyword{models}% at least one, from doc/KEYWORDS


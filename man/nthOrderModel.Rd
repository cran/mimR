\name{nthOrderModel}
\alias{nthOrderModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create generating class with nth order interactions for log--linear model}
\description{
  Creates  generating class with nth order interactions for log--linear
  model
  }
\usage{
nthOrderModel(variables, order = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variables}{A vector of variables}
  \item{order}{The maximum order of interactions in the model}
}
\value{
  A string
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}
\examples{
nthOrderModel(c("ss","uu","ii"), 2)
}
\keyword{models}% at least one, from doc/KEYWORDS


\name{MIMFORMULA}
\alias{parse.mim.formula}
\alias{as.mim.formula.object}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Creating MIM formula objects}
\description{
  For internal use. A MIM formula is a structured representation of the
  formula string used in MIM.
}
\usage{
parse.mim.formula(mim.formula, names.table = NULL, mim.names=TRUE)

as.mim.formula.object(mim.formula = NULL, block.list = NULL, names.table = NULL, mim.names = TRUE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{mim.formula}{}
  \item{names.table}{}
  \item{mim.names}{}
  \item{block.list}{}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  A mim.model object
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Second Edition, Springer Verlag, 2000}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

% ~Make other sections like WARNING with \section{WARNING }{....} ~

%\seealso{ ~~objects to SEE ALSO as \code{\link{~~fun~~}}, ~~~ }

\examples{
}
\keyword{utilities}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line



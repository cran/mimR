\name{fixEdges}
\alias{fixEdges}
\alias{fixSet}

\title{Fixing edges and sets in a mim model}
\description{
  Fix edges and sets in a mim model.
}
\usage{
fixEdges(v=NULL, mim=NULL)
fixSet(v=NULL, mim=NULL)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{An edge/vertex set, given as "a:b+c:d" or "a:b,c:d"}
  \item{mim}{A mim object}
}

\details{
 If v=NULL then al fixes are removed. Only in this case the mim argument
 needs not to be given.
}

\value{
None
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}


\seealso{See Also as \code{\link{stepwise}}}
%\examples{
%}
\keyword{models}% at least one, from doc/KEYWORDS


\name{mim.setblock}
\alias{mim.setblock}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Set block recursive structure in MIM}
\description{
  Set block recursive structure in MIM
  }
\usage{
mim.setblock(br.structure = NULL, data = NULL, mim.names = TRUE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{br.structure}{}
  \item{data}{ }
  \item{mim.names}{  }
}
\details{
  If br.structure is NULL, the block recursive structure is removed.
  }
\value{
  A string with the block recursive structure
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

\seealso{\code{\link{mim.br.model}}}

\examples{
data(rats)
md <-mim.read(rats, mim.names=c("a","b","x","y"),mim.labels=names(rats))
mim.setblock("Sex,Drug|W1|W2",data=md,mim.names=FALSE)
mim.cmd("show b")
mim.setblock(c("Sex,Drug","W1","W2"),data=md,mim.names=FALSE)
mim.setblock("ab|x|y",data=md)
mim.setblock(c("ab","x","y"),data=md)
mim.setblock()
}
\keyword{models}% at least one, from doc/KEYWORDS


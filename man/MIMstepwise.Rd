\name{stepwise}

\alias{stepwise}
\alias{stepwise.mim}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Stepwise model selection in MIM}
\description{
  Functions to do stepwise model selection in MIM to achieve a new model
  object.  }

\usage{
stepwise(x, arg = NULL)
}
%- maybe also `usage' for

\arguments{
  \item{x}{A mimModel object}
  \item{arg}{Stepwise options to MIM}
}
\value{
  a mimModel object
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

%\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }

\examples{
data(carcass)
gmd.carc <- as.gmData(carcass)

m.main <- fit(mim(~.,  data=gmd.carc))
m.sat  <- fit(mim(~..,  data=gmd.carc))

m.main <- mim(~.,  data=gmd.carc)
m.sat  <- mim(~..,  data=gmd.carc)

m.m <- stepwise(m.main, "f")    # forward
m.s <- stepwise(m.sat, "s")     # backward, exact tests
}
\keyword{models}% at least one, from doc/KEYWORDS


\name{stepwise}

\alias{stepwise}
\alias{stepwise.mim}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Stepwise model selection in MIM}
\description{
  Functions to do stepwise model selection in MIM to achieve a new model
  object.  }

\usage{
stepwise(x, arg = NULL, critlevel=NULL, infconstant=NULL)

}
%- maybe also `usage' for

\arguments{
  \item{x}{A mimModel object}
  \item{arg}{Stepwise options to MIM}
  \item{critlevel}{Set the critical level for the model
    selection. Default is 0.05}
  \item{infconstant}{Penalizing parameter used when model selection in
    based on information criteria, see 'details' below.}
}
\details{
  Setting arg to contain "A" leads to model selection by AIC, i.e. the
  model with the smallest value of -2log Q - 2 * p is choosen. With BIC,
  the model with the smallest value of  -2log Q - log(n) * p is chosen.
  Setting infconstant to some value k AND setting arg to contains "A"
  will lead to selecting the model  with the smallest value of  -2log Q
  - k * p. 
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

m.main <- fit(mim(".",  data=gmd.carc))
m.sat  <- fit(mim("..",  data=gmd.carc))

m.main <- mim(".",  data=gmd.carc)
m.sat  <- mim("..",  data=gmd.carc)

m.m <- stepwise(m.main, "f")    # forward
m.s <- stepwise(m.sat, "s")     # backward, exact tests
}
\keyword{models}% at least one, from doc/KEYWORDS


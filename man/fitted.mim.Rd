\name{fitted.mim}
\alias{fitted.mim}
\alias{modelInfo}
\alias{print.modelInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract fitted values (parameter estimates) }
\description{
  Extracts fitted values (parameter estimates) from a mim model.
}
\usage{
fitted.mim(object, ...)
modelInfo(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ A mim model object}
  \item{\dots}{ Other argumens}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  A data frame
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Second Edition, Springer Verlag, 2000}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

% ~Make other sections like Warning with \section{Warning }{....} ~ 
\seealso{\code{\link{simulate}}}
\examples{
data(rats)
gmd.rats <- as.gmData(rats)
m2    <- mim("ab/abc+abd/cd", data=gmd.rats, letter=TRUE)
mf2   <- fit(m2)
parms <- fitted(mf2)
parms


m3    <- mim("a,b", data=gmd.rats, letter=TRUE)
mf3   <- fit(m3)
parms <- fitted(mf3)
parms

m4    <- mim("//cd", data=gmd.rats, letter=TRUE)
mf4   <- fit(m4)
parms <- fitted(mf4)
parms
}
\keyword{models}% at least one, from doc/KEYWORDS


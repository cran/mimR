\name{simulate}
\alias{simulate}
\alias{simulate.mim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw random sample from a mixed interaction model (mim)}
\description{
  This function draws a random sample from a fittet mim model.
}
\usage{
simulate(mim, size, digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mim}{A mim model object}
  \item{size}{The size of the sample}
  \item{digits}{The number of digits for the continuous variables}
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
\seealso{\code{\link{fitted}}}
\examples{
data(rats)
gmd.rats <- as.gmData(rats)
m2   <- mim("ab/abc+abd/cd", data=gmd.rats, letter=TRUE)
mf2 <- fit(m2)
#samp <- simulate(mf2, size=10)   # 'sample' is already used in R 
#samp 
}
\keyword{datagen}% at least one, from doc/KEYWORDS

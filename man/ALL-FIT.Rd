\name{FIT}
\alias{mim.fit}
\alias{mim.emfit}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Function to fit model in MIM}
\description{
  Fits model in MIM either directly or using an EM--algorithm in the
  case of incomplete data
}
\usage{
mim.fit()
mim.emfit(arg = "R", plot = FALSE)
}
\arguments{
  \item{arg}{ can be one of the letters "R", "S" or "F"}
  \item{plot}{ if TRUE, a plot of the convergence history of the EM
    algorithm is made.}
}
\details{
  The S option enables the user to supply start values for the missing
  data: as with the R option these are used to calculate the sufficient
  statistics, and thence the initial parameter estimates.
  First start values are entered, and then these are overwritten 
  with an asterix (missing value).
  For an example of how to do this, see the examples below.

  The start values can be entered using EditData in MIM: first enter the desired
  value, and then overwrite this with an asterix (missing value). Check
  using Print E in MIM that the values have been correctly entered.
}
\value{
  mim.fit: Returns a vector with deviance and degrees of freedom
  mim.emfit: A dataframe with the convergence history of the EM algorithm.
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

%~Make other sections like WARNING with \section{WARNING }{....} ~

%\seealso{\code{\link{mim.emfit}} }

\examples{
data(carcass)
mim.read(carcass, mim.names=c("a","b","c","d","e","f","y"),mim.labels=names(carcass))

mim.cmd("model //ay,by,cy,dy,ey,fy")
mim.fit()

mim.cmd("model //abcdefy")
mim.fit()

data(carcass)
mim.read(carcass, mim.names=c("a","b","c","d","e","f","y"))

mim.cmd("cont L; calculate L=ln(0)");
mim.cmd("model //aL,bL,cL,dL,eL,fL,yL")
mim.emfit()

# Insert initial values for EM algorithm
#carcass$L <- rnorm(nrow(carcass),sd=0.1)
#mim.read(carcass, mim.names=c("a","b","c","d","e","f","y","L"))
#mim.cmd("calculate L=ln(0)");
#mim.cmd("model //aL,bL,cL,dL,eL,fL,yL")
#mim.emfit("S")
}
\keyword{models}% at least one, from doc/KEYWORDS
%\keyword{utilities}
%\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line





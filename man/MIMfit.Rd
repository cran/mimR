\name{FIT}
\alias{fit}
\alias{emfit}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Function to fit model in MIM}
\description{
  Fits model in MIM either directly or using an EM--algorithm in the
  case of incomplete data
}
\usage{
fit(mim, submitData=TRUE)
emfit(mim, arg = "R", submitData=TRUE, emconv=0.0001, emmax=1000, plot = FALSE,info=FALSE) %
}
\arguments{
  \item{mim}{ A mim model object}
  \item{submitData}{If TRUE  data are submitted to MIM, otherwise
    not. If data is already loaded in MIM, setting submitData=FALSE
    saves some time}
  \item{arg}{ can be one of the letters "R", "S" or "F"}
  %\item{plot}{ if TRUE, a plot of the convergence history of the EM
  %  algorithm is made.}
  \item{emconv}{Convergence criterion for EM algorithm}
  \item{emmax} {Maximum number of iterations for EM algorithm}
  \item{plot}{If TRUE, the convergence history of the EM algorithm is
    plotted}
  \item{info}{If TRUE, information about EM fit is printed}
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
  Returns a fitted mim object
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

%~Make other sections like WARNING with \section{WARNING }{....} ~

%\seealso{\code{\link{mim.emfit}} }

\examples{
data(mathmark)
math     <- mathmark
math$L   <- factor(NA, levels=1:2)
gmd.math <- as.gmData(math)
latent(gmd.math) <- "L"
m1  <- mim("..", data=gmd.math)
m2  <- editmim(m1, deleteEdge=paste(names(math)[1:5],collapse=':')) 
m2f <- emfit(m2)
imputeMissing()
d.imp <- retrieveData()
}
\keyword{models}% at least one, from doc/KEYWORDS
%\keyword{utilities}
%\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line





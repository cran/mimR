\name{LINPREDICT}
\alias{linpredict}
\alias{linpredict.mim}
\alias{displayMIM}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{mimR version of MIM command 'display'}
\description{
  This function returns the conditional mean and covariance of y given x
  if y is coninuous and the linear predictor of y given x if x is
  discrete.
}
\usage{
linpredict(mim,y,x=NULL,letter=FALSE)
displayMIM(y, x=NULL)


}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{mim}{A mim model object}
  \item{y}{ Vector of response variables}
  \item{x}{ Vector of explanatory variables}
  \item{letter}{If TRUE then x and y contains the variables as letters
    (instead of as proper names)}
}
\details{
  The implementation of linpredict is slightly fragile and certainly not
  elegant. Please use it with caution.
  The displayMIM function is not intended for the user
}
\value{
  Returns a linpredictMIM object which is a list of lists containing the
  quantities returned.
}

\references{ David Edwards, An Introduction to Graphical Modelling,
  Second Edition, Springer Verlag, 2000}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

\section{WARNING }{
If a and b are discrete and x and y are coninuous, then the MIM command
display by,ax will return the linear predictor of b given a,x and the
conditional mean and variance of y given a,x.

In mimR, on the other hand, the response types can not be mixed, so you
should do it in two steps.
} 

\seealso{\code{\link{fitted}}}

\examples{
data(rats)
gmd.rats <- as.gmData(rats)
m1   <- mim("Sex:Drug/Sex:Drug:W1+Sex:Drug:W2/W1:W2", data=gmd.rats)
mf1  <- fit(m1)
d1  <- linpredict(mf1, y="W2",    x="W1:Sex") 
d2  <- linpredict(mf1, y="Sex",   x="W1:W2")
d3  <- linpredict(mf1, y="W1:W2", x="Sex")
d4  <- linpredict(mf1, y="W1:W2")
d5  <- linpredict(mf1, y="Sex")
d1; d2; d3; d4; d5
}
\keyword{models}
%\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line












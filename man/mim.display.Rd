\name{mim.display}
\alias{mim.display}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{mimR version of MIM command 'display'}
\description{
  This function returns the conditional mean and covariance of y given x
  if y is coninuous and the linear predictor of y given x if x is
  discrete.
}
\usage{
mim.display(y, x)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{y}{ Vector of response variables}
  \item{x}{ Vector of explanatory variables}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  Returns a mim.display object which is a list of lists containing the
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
should do it in two steps:

mim.display("b",c("a","x"));
mim.display("y",c("a","x"))

} 

\seealso{ \code{\link{mim.read}}, }

\examples{
data(rats)
mim.cmd("clear;clear output")
mnt <-mim.read(rats, mim.names=c("a","b","x","y"),mim.labels=names(rats))
mim.cmd("satmod; fit")

mim.display("y")
mim.display("y","x")
mim.display("y","a")
mim.display("y","ax")

mim.display("b")
mim.display("b","x")
mim.display("b","a")
mim.display("b","ax")

mim.cmd("clear;clear output")
rats$z     <- round(rnorm(length(rats$W1),sd=2, mean=2))
mnt <-mim.read(rats, mim.names=c("a","b","x","y","z"),mim.labels=names(rats))
mim.cmd("mod ab/abx,aby,abz/abxyz; fit"); 

mim.display("y","ax") 
mim.display("b","ax") 
mim.display("ab","x") 
mim.display("xy","a") 
mim.display("xy","az")
}
\keyword{models}
%\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line












\name{retrieveData}
\alias{retrieveData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Retrieve data from MIM engine}
\description{
  This function can be used for retrieving data from the MIM engine. It
  is useful in connection with latent variable models
}
\usage{
retrieveData(arg="c")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{arg}{"c": the raw data (using value labels), "d": the raw data
(using levels) , "e": the raw data (showing missing values)}

}
%\details{
%}
\value{
  A data frame
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

\seealso{See Also as \code{\link{fit.mim}}}
%\examples{
%}
\keyword{utilities}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

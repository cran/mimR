\name{retrieveData}
\alias{retrieveData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Retrieve data from MIM engine}
\description{
  This function can be used for retrieving data from the MIM engine. It
  is useful in connection with latent variable models
}
\usage{
retrieveData(gmData = NULL, impute = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gmData}{A gmData object}
  \item{impute}{If TRUE, missing values are imputed (from the fitted
    model) before the data is returned}

}
\details{
  The gmData object is used only for naming the columns of the data
  frame.
}
\value{
  A data frame
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

\seealso{See Also as \code{\link{emfit}}}
\examples{
}
\keyword{utilities}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

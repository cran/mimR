\name{returnValues}
\alias{returnValues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Return fitted and observed values}
\description{
Returns fitted and observed values together with deviance etc. for MIM
model objects
}
\usage{
returnValues(object, type = "fitted", marginal = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A mim model object}
  \item{type}{Can be either "fitted", "observed", "deviance" or
    "pearson" (the two latter only for discrete models)}
  \item{marginal}{A subset of the variables of the model}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
A vector
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Second Edition, Springer Verlag, 2000}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

% ~Make other sections like Warning with \section{Warning }{....} ~ 
%\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
}
\keyword{ models }% at least one, from doc/KEYWORDS

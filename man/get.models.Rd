\name{get.models}
\alias{get.models}
\alias{get.models.mimData}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Get the model list from a mimData object}
\description{
  The model list is a list of models organised as mim.model objects.
}
\usage{
get.models(mimData, model = NULL, short = FALSE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{mimData}{A mimData object}
  \item{model}{A number in the model list. If null, all models are
    returned. }
  \item{short}{if TRUE only a short summary is printed.}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  A (list of) mim.model objects
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}
% ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{\code{\link{as.mimData}}}

\examples{
}
\keyword{models}% at least one, from doc/KEYWORDS

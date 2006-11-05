\name{show.mim}
\alias{display}
\alias{display.mim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Display a 'mim' model graphically}
\description{
  Dislays a model graphically. This function is highly experimental and
  requires that the packages 'Rgraphviz' and 'graph' are loaded
}
\usage{
display(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A mim model object}
%  \item{...}{Additional arguments which are not used.}
}
\details{
  None
}
\value{
  None
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{mim}}}
\examples{

\dontrun{
## Requires the Rgraphwiz and the graph packages.
library(Rgraphviz)
library(graph)
data(rats)
gd <- as.gmData(rats)
m12   <- mim("Sex:Drug/Sex:Drug:W1+Sex:Drug:W2/W1+W2", data=gd)
display(m12)
}


}
\keyword{hplot}% at least one, from doc/KEYWORDS


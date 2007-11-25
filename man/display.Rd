\name{display}
\alias{display,mim-method}
\alias{display.mim}
\alias{display}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Display a 'mim' model graphically}
\description{
  Dislays a model graphically. This function is highly experimental and
  requires that the packages 'Rgraphviz' and 'graph' are loaded
}
\usage{
display(x)
\S4method{display}{mim}(x)
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
  The graph object. 
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link{mim}}}
\examples{

data(rats)
gd <- as.gmData(rats)
m12   <- mim("Sex:Drug/Sex:Drug:W1+Sex:Drug:W2/W1+W2", data=gd)
plot(m12)

}
\keyword{hplot}% at least one, from doc/KEYWORDS


\name{create.table}
\alias{create.table}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Create dataframe for a contingency table}
\description{
  Creates a dataframe with 6 rows for a 2x3 table.
}
\usage{
create.table(levels, names = NULL)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{levels}{A vector of levels}
  \item{names}{A vector with names}
}
\value{
A dataframe
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}
% ~Make other sections like WARNING with \section{WARNING }{....} ~

%\seealso{ ~~objects to SEE ALSO as \code{\link{~~fun~~}}, ~~~ }

\examples{
create.table(c(2,3,2),c("a","b","c"))
}
\keyword{utilities}% at least one, from doc/KEYWORDS


\name{MISCUTIL}
\alias{silent.as.numeric}
\alias{float.to.string}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Miscellaneous utility functions for mimR}
\description{
  silent.as.numeric: Turns c("hello", "123", "0.234") into  NA 123.000   0.234
  without producing a warning.

  float.to.string: Converts floats (reals) to strings while dropping
  exponential notation

  
}
\usage{
silent.as.numeric(string.vec)
float.to.string(num.vec, n.digits = 6, width = 9, preserve.int = T)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{string.vec}{Vector of strings}
  \item{num.vec}{A vector of numbers to be turned into strings. The number
    1e-3 will be turned into 0.001.}
  \item{n.digits}{Number og digits to output}
  \item{width}{Width of the output strings}
  \item{preserve.int}{If TRUE, then an integer in arg will be preserved
    as an integer in the output.}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  Vector of numbers and NAs or vector of strings
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}
% ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{\code{\link{float.to.string}}}

\examples{
silent.as.numeric(c("hello", "123", "0.234"))

float.to.string(c(12,1e-3,1.232,-2))
}
\keyword{utilities}% at least one, from doc/KEYWORDS


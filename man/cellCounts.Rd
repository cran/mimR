\name{cellCounts}
\alias{cellCounts}
\alias{empCov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convenience functions for specifying sufficient statistics for
  purely discrete and purely continuous graphical models}
\description{
  These functions provide a way of specifying a contingency table by a
  vector of counts and for specifying the sufficient statistics for a
  covariance selection model as a covariance matrix.
}
\usage{
cellCounts(varNames, nLevels = NULL, valueLabels = NULL, observations)
empCov(S, counts = NULL, sd = NULL, mean = rep(0, ncol(S)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{varNames}{ ~~Describe \code{varNames} here~~ }
  \item{nLevels}{ ~~Describe \code{nLevels} here~~ }
  \item{valueLabels}{ ~~Describe \code{valueLabels} here~~ }
  \item{observations}{ ~~Describe \code{observations} here~~ }
  \item{S}{ ~~Describe \code{S} here~~ }
  \item{counts}{ ~~Describe \code{counts} here~~ }
  \item{sd}{ ~~Describe \code{sd} here~~ }
  \item{mean}{ ~~Describe \code{mean} here~~ }

}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
x <- cellCounts(varNames=c("aa","bb"), 
  valueLabels=list(aa=c("a1","a2"), bb=c("b100","b200")), 
  observations=c(1,2,3,4))

as.gmData(x)


S <- structure(c(305.77, 127.22, 101.58, 106.27, 117.4, 127.22, 172.84, 
85.16, 94.67, 99.01, 101.58, 85.16, 112.89, 112.11, 121.87, 106.27, 
94.67, 112.11, 220.38, 155.54, 117.4, 99.01, 121.87, 155.54, 
297.76), .Dim = c(5L, 5L), .Dimnames = list(c("me", "ve", "al", 
"an", "st"), c("me", "ve", "al", "an", "st")))

x <- empCov (S,88)

as.gmData(x)
  
  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

\name{mim.statread}
\alias{mim.statread}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Enters a set of sufficient statistics for MIM}
\description{
  The set of sufficient statistics is represented by a data structure
  whose form depends on the variables. See examples below.
}
\usage{
mim.statread(stat)
}
\arguments{
  \item{stat}{\code{stat} is an object of type mim.parameters (usually
    created by make.mim.stats())}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  The result from MIM after reading the data.
  
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}
\seealso{\code{\link{get.mim.parameters}}, \code{\link{make.mim.stats}} }

\examples{
v   <- matrix(c(1.5,2.1,2.1,7.3),ncol=2)
s   <- list(v,v,v,v)
m   <- list(c(5,6),c(8,9),c(12,13),c(16,17))
d   <- c(5,7,9,12)

s1  <- make.mim.stats(disc.names=c("a","b"),disc.levels=c(2,2), counts=d)

s2  <- make.mim.stats(cont.names=c("y","z"), counts=8, means=c(5,6),cov=v)

s3  <- make.mim.stats(disc.names=c("a","b"),disc.levels=c(2,2), cont.names=c("y","z"),
        means=m, cov=s, counts=d)

mim.statread(s1)
mim.statread(s2)
mim.statread(s3)
}
\keyword{models}% at least one, from doc/KEYWORDS

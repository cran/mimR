\name{make.mim.stats}
\alias{make.mim.stats}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Creates a set of sufficient statistics for MIM}
\description{
    The set of sufficient statistics is represented by a data structure
  whose form depends on the variables. See examples below. The
  statistics will usually be entered into MIM using the mim.statread()
  function.
}
\usage{
make.mim.stats(disc.names = NULL, cont.names = NULL, disc.levels = NULL, means = NULL, cov = NULL, counts = NULL, stats = NULL, table = NULL)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{disc.names}{\code{disc.names}  }
  \item{cont.names}{ \code{cont.names} }
  \item{disc.levels}{ \code{disc.levels} }
  \item{means}{ \code{means} }
  \item{cov}{ \code{cov} }
  \item{counts}{  \code{counts}  }
  \item{stats}{  \code{stats}  }
  \item{table}{  \code{table}  }
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  An object of type mim.parameters
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

\seealso{\code{\link{get.mim.parameters}}, \code{\link{mim.statread}} }


% ~Make other sections like WARNING with \section{WARNING }{....} ~

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







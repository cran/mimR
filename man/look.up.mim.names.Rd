\name{look.up.mim.names}
\alias{look.up.mim.names}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{For internal use}
\description{
  For internal use
}
\usage{
look.up.mim.names(terms, names.table, direction = "from.mim")
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{terms}{}
  \item{names.table}{}
  \item{direction}{}
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

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\%seealso{ ~~objects to SEE ALSO as \code{\link{~~fun~~}}, ~~~ }

\examples{
data(rats)
mnt <-mim.read(rats, mim.names=c("a","b","x","y"),mim.labels=names(rats))

look.up.mim.names("abx,aby", names.table=mnt$names.table,direction="from.mim")
look.up.mim.names("abx", names.table=mnt$names.table,direction="from.mim")
look.up.mim.names("x", names.table=mnt$names.table,direction="from.mim")

look.up.mim.names("Sex:Drug:W1,Sex:Drug:W2", names.table=mnt$names.table,direction="to.mim")
look.up.mim.names("Sex:Drug:W1", names.table=mnt$names.table,direction="to.mim")
look.up.mim.names("W1", names.table=mnt$names.table,direction="to.mim")
}
\keyword{utilities}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line








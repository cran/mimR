\name{editMIM}
\alias{editmim}
\alias{testdelete}
\alias{retrieveMIMvalues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Editing a mim model object}
\description{
  Editing a mim model object by adding/deleting edges.
}
\usage{
editmim(x, add = NULL, hadd = NULL, del = NULL, letter = FALSE)
testdelete(edge, mim, options=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A mim model object }
  \item{add}{Edges to be added}
  \item{hadd}{Edges to be added (homogeneously)}
  \item{del}{Edges to be deleted}
  \item{letter}{If TRUE, the variables are specified using single
    letters.}
  \item{edge}{Edge to be tested}
  \item{mim}{A mim object}
  \item{options}{Additional arguments to MIM specifying the tests}
}
\details{
  retrieve is used for retrieving a model (as a mim object) manually
  from MIM after e.g. altering a model in the MIM program directly
}
\value{
  A new mim model object
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Second Edition, Springer Verlag, 2000}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

 ~Make other sections like Warning with \section{Warning }{....} ~ 
%\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
data(rats)
gmd.rats <- as.gmData(rats)
m.main <- mim(".",  data=gmd.rats)
m2 <- editmim(m.main, add=c("Sex:Drug", "Sex:W2"))
m3 <- editmim(m.main, add=c("Sex:Drug", "Sex:W2"),hadd="Drug:W1:W2")

summary(m2)
summary(m3)
}
\keyword{models}% at least one, from doc/KEYWORDS

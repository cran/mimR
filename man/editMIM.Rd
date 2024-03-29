\name{editMIM}
\alias{editmim}
\alias{update.mim}
\alias{testdelete}
\alias{retrieveMIMvalues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Editing a mim model object}
\description{
  Editing a mim model object by adding/deleting edges.
}
\usage{
editmim(object, deleteEdge=NULL, addEdge=NULL, haddEdge=NULL,
                    deleteTerm=NULL, addTerm=NULL,fit=object$fit)
\method{update}{mim}(object, deleteEdge=NULL, addEdge=NULL, haddEdge=NULL,
                    deleteTerm=NULL, addTerm=NULL,fit=object$fit,...)
testdelete(edge, object, arg=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A mim model object }
  \item{deleteEdge}{Edges to be deleted}
  \item{addEdge}{Edges to be added}
  \item{haddEdge}{Edges to be added (homogeneously)}
  \item{deleteTerm}{Terms to be deleted}
  \item{addTerm}{Terms to be added}
  \item{...}{Additional arguments to update. Currently not used.}
  \item{fit}{Should the updated model be fitted}
  \item{edge}{Edge to be tested}
  \item{arg}{Additional arguments to MIM specifying the tests}
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
\author{S�ren H�jsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

% ~Make other sections like Warning with \section{Warning }{....} ~ 
\seealso{See Also as \code{\link{modelTest}}}
\examples{
data(rats)
gmd.rats <- as.gmData(rats)
m.main   <- mim(".",  data=gmd.rats)
m2 <- editmim(m.main, addEdge=c("Sex:Drug", "Sex:W2"))
m3 <- editmim(m.main, addEdge=c("Sex:Drug", "Sex:W2"),haddEdge="Drug:W1:W2")

summary(m2)
summary(m3)

data(housing)

housingTab <- xtabs(Freq ~ Sat + Infl + Type + Cont, data = housing)
ht <- as.gmData(housingTab)

marg1 <- mim("Sat:Infl:Cont//", data=ht)
testdelete("Sat:Cont",marg1)

## Try deleting an edge not in the model:

testdelete("Sat:Type",marg1)
testdelete("Sat:Cont",marg1,arg="MJ")
testdelete("Sat:Cont",marg1,arg="M")


}
\keyword{models}% at least one, from doc/KEYWORDS

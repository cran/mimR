\name{cliques}
\docType{methods}
\alias{cliques}
%\alias{nodes}
\alias{cliques,mim-method}
%\alias{nodes,mim-method}
%\alias{Edges.mim}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get cliques, edges etc of MIM model objects}
\description{
  Get cliques, edges etc of MIM model objects
}
\usage{
\S4method{cliques}{mim}(object, which)
%\S4method{nodes}{mim}(object, which)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A MIM model object}
  \item{which}{Not used}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A list
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

data(carcass)
gmd.carc <- as.gmData(carcass)
m.sat  <- fit(mim("..",  data=gmd.carc))
cliques(m.sat)
nodes(m.sat)
edges(m.sat)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

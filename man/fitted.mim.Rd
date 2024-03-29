\name{fitted.mim}
\alias{fitted.mim}
\alias{modelInfo}
\alias{print.modelInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract fitted values (parameter estimates) }
\description{
  Extracts fitted values (parameter estimates) from a mim model.
}
\usage{
\method{fitted}{mim}(object, data.frame=FALSE, ...)
modelInfo(object,slot=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ A mim model object}
  \item{data.frame}{If the model is discrete (a log linear model) then fitted
  values are returned as a table by default. Setting data.frame=TRUE
  imples that fitted values are returned as a dataframe with the column
  'Freq' containg the fitted values.}
  
  \item{\dots}{ Other argumens}
  \item{slot}{A specific slot of the modelInfo list. If NULL, the entire
  list is returned.}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  A data frame
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Second Edition, Springer Verlag, 2000}
\author{S�ren H�jsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

% ~Make other sections like Warning with \section{Warning }{....} ~ 
\seealso{\code{\link{simulate}}}
\examples{
data(rats)
gmd.rats <- as.gmData(rats)
m2    <- mim("..", data=gmd.rats)
mf2   <- fit(m2)
parms <- fitted(mf2)
parms
}
\keyword{models}% at least one, from doc/KEYWORDS


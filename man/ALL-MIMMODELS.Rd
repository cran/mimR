\name{MIMMODELS}
\alias{mim.model}
\alias{mim.br.model}
\alias{submit.model}
\alias{submit.model.mimModel}
\alias{submit.model.mimBRModel}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Create undirected and block recursive MIM models }
\description{
  ...........  }


\usage{

mim.model(mim.formula, data, mim.names = TRUE,
submit = TRUE, submit.data=TRUE)

mim.br.model(mim.formula, data,
mim.names = TRUE, submit = TRUE, submit.data=TRUE)

submit.model(x,submit.data=TRUE)
}

%- maybe also `usage' for other objects documented here.


\arguments{
  \item{mim.formula}{A text string following the MIM syntax. Long
    variable names are allowed however. See 'details'.}
  \item{data}{A mimData object}
  \item{mim.names}{If TRUE, the variables used in mim.formula are single
  letters. If FALSE the variables are the names of the mimData (frame)}
\item{submit}{If TRUE, the model is submitted to MIM}
\item{submit.data}{If TRUE, the data associated with the mim model is
  also submitted to MIM}
\item{x}{A mimModel or mimBRModel object}
} 
\details{
  A mim.formula can be either "ab/abx,aby/abxy" (if mim.names is TRUE)
  or "Sex, Drug / Sex:W1,Drug:W1, Sex:W2,Drug:W2 / Sex:W1:W2,Drug:W1:W2"
  (if mim.names is FALSE). See 'examples'.
}
\value{
  A mimModel or mimBRModel object
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{\code{\link{as.mimData}}, \code{\link{mim.setblock}}}

\examples{
}
\keyword{models}% at least one, from doc/KEYWORDS







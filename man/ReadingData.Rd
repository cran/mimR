\name{mimR-ReadingData}

\alias{gmData}
\alias{as.gmData}
\alias{as.gmData.data.frame}
\alias{as.gmData.table}
\alias{as.gmData.suffStats}

\alias{description}
\alias{description.gmData}
\alias{description<-}
\alias{description<-.gmData}

\alias{latent}
\alias{latent.gmData}
\alias{latent<-}
\alias{latent<-.gmData}

\alias{vallabels}
\alias{vallabels.gmData}
\alias{vallabels<-}
\alias{vallabels<-.gmData}

\alias{observations}
\alias{obs}
\alias{observations.gmData}
\alias{observations<-}
\alias{observations<-.gmData}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Creation of gmData objects}
\description{
  Different methods for creation of gmData objects. Depending on taste
  gmData can be taken as meaning 'graphical model data' or 'graphical
  meta data'
}
\usage{
gmData(name, letter = c(letters, LETTERS), factor = rep(FALSE,
length(name)), vallabels=NULL, data=NULL)
as.gmData (data, letter=c(letters,LETTERS))
description(x)
"description<-"(tmp, value)
latent(x)
"latent<-"(tmp, value)
vallabels(x)
"vallabels<-"(tmp, value)
observations(x)
"observations<-"(tmp, value)

}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{name}{ ~~Describe \code{name} here~~ }
  \item{factor}{ ~~Describe \code{factor} here~~ }
  \item{data}{a dataframe}
  \item{letter}{A specification of letters which can subsequently
    be used as short hand for the variable names}
  \item{x}{A gmData object}
  \item{tmp}{A gmData object}
  \item{value}{The value to entered into the object}
  %\item{text}{A text string}
  \item{vallabels}{A list containing labels of the levels of the
    discrete variables (not currently used)}
  %\item{variables}{A set of variables}
  %\item{obs}{Data to be used in the object. Can be a data frame, a
  %  table or a set of sufficient statistics (as used by MIM)}
  %\item{labels}{A list in which each item consists of a vector with
  %  labels of the factors in the gmData object. See examples below.}
}
\details{
 as.mimData is just a wrapper for as.gmData (kept in the package for
 historical reasons). as.mimData only works for data frames.
}
\value{
  A dataframe containing the link between the names in the dataframe and
  the names (and labels) and the coding of factors in MIM
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Second Edition, Springer Verlag, 2000}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

\section{WARNING}{
If only a subset of a dataframe is used, then it may be necessary to
recode the levels of factors in the dataframe. Otherwise one can end up with
slices of the table in MIM with no observations in it. The easiest way
to recode the levels is to use the function 'factor()', see examples below.
} 

\seealso{\code{\link{mim.cmd}}}

\examples{
# A meta data object (without data)
gmd.rats.nodata  <-  gmData(c("Sex","Drug","W1","W2"),
    factor=c(2,3,FALSE,FALSE),
    vallabels=list("Sex"=c("M","F"), "Drug"=c("D1","D2","D3")))

# Note care should be taken to be consistent when specifying vallabels manually:

#This works
nt1  <-  gmData(c("Sex","Drug","W1","W2"),factor=c(2,3,FALSE,FALSE))
vallabels(nt1)

#This works
nt12  <-  gmData(c("Sex","Drug","W1","W2"),factor=c(2,3,FALSE,FALSE), vallabels=list("Sex"=c(1,2), "Drug"=c(1,2,3)))
vallabels(nt1)

\dontrun{
#This will not work
nt1  <-  gmData(c("Sex","Drug","W1","W2"),factor=c(2,3,FALSE,FALSE), vallabels=list("Sex"=c(1,2,3), "Drug"=c(1,2,3)))
vallabels(nt1)

#This will not work
nt1  <-  gmData(c("Sex","Drug","W1","W2"),factor=c(2,3,FALSE,FALSE), vallabels=list("Sex"=c(1,2,3)))
vallabels(nt1)
}


data(rats)

#NOTE: This works, but it should not because names in 'data' is not checked for consistency with what is specified
nt1  <-  gmData(c("Sex","Drug","W1","W2"),factor=c(2,3,FALSE,FALSE), vallabels=list("Sex"=c(1,2), "Drug"=c(1,2,3)), data=rats)
vallabels(nt1)

# Sending a data frame to MIM
gm.rats <- as.gmData(rats)
toMIM(gm.rats)
}
\keyword{models}% at least one, from doc/KEYWORDS
%\keyword{utilities}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line


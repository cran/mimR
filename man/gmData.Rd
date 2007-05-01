\name{gmData}

\alias{gmData}
\alias{as.gmData}
\alias{as.gmData.data.frame}
\alias{as.gmData.table}
\alias{as.gmData.momentstats}

\alias{description}
\alias{description.gmData}
\alias{description<-}
\alias{description<-.gmData}

\alias{ordinal<-}
\alias{ordinal<-.gmData}
\alias{ordinal}
\alias{ordinal.gmData}

\alias{nominal<-}
\alias{nominal<-.gmData}
\alias{nominal}
\alias{nominal.gmData}


\alias{latent}
\alias{latent.gmData}
\alias{latent.mim}
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
gmData(name,  factor = rep(FALSE,
length(name)), vallabels=NULL)
as.gmData (data)
description(x)
"description<-"(tmp, value)
latent(x)
"latent<-"(tmp, value)
vallabels(x)
"vallabels<-"(tmp, value)
observations(x)
"observations<-"(tmp, value)

"ordinal<-"(tmp, value)
ordinal(x)

"nominal<-"(tmp, value)
nominal(x)

}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{name}{ ~~Describe \code{name} here~~ }
  \item{factor}{ ~~Describe \code{factor} here~~ }
  \item{data}{a dataframe}
  %\item{letter}{A specification of letters which can subsequently
  %  be used as short hand for the variable names}
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
  A variety of data formats can be turned into gmData objects. See
  examples. 
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
}
\keyword{models}% at least one, from doc/KEYWORDS
%\keyword{utilities}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line


\name{ReadingData}

\alias{mim.read}
\alias{as.mimData}
\alias{submit.mimData}
\alias{print.mimData}
\alias{save.mimData}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Read a data frame into MIM}
\description{
  This function takes a dataframe and writes it onto a file in the
  format needed by MIM. That is, header information about the levels of
  factors etc. are added. 
}
\usage{
mim.read  (data, mim.names = NULL, mim.labels = NULL, file = FALSE,
submit = TRUE, n.digits = 12, title=NULL) 
as.mimData(data, mim.names = NULL, mim.labels = NULL, file = FALSE,
submit = TRUE, n.digits = 12, title=NULL) 
submit.mimData(mimData, file = NULL, submit = TRUE, n.digits = 12)
save.mimData(x, file="mimData")
%print.mimData(x, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{data}{a dataframe }
  \item{mimData}{}
  \item{mim.names}{Vector of letters to be used as names for the
    variables in MIM. If NULL, it is first checked if the names of m are
    all just a single letter. If so the names of m are used in MIM. If one
    of the columns of m has a name of length larger than one, then the
    letters are used, i.e. a,b,c,...., X,Y,Z.}
  \item{mim.labels}{Vector of labels to be used in graphs in MIM }
  \item{file}{The file onto which the data frame should be written. If
    NULL, a temporary file will be created.}
  \item{submit}{If TRUE the file will be read into MIM}
  \item{n.digits}{ Number of digits to represent the data with on the
    file.}
  \item{title}{A textstring with a name of the mimData object.}
  \item{x}{A mimData object}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
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
data(rats)
mim.read(rats)

mim.read(rats, mim.names=c("a","b","x","y"), mim.labels=names(rats))
## is equivalent to
mim.read(rats, file="c:\\mimrats.txt", submit=FALSE,
mim.names=c("a","b","x","y"), mim.labels=names(rats))
mim.cmd("input c:\\mimrats.txt")

}
\keyword{models}% at least one, from doc/KEYWORDS
%\keyword{utilities}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line


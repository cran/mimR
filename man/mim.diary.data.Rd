\name{mim.diary.data}
\alias{mim.diary.data}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Function to get a listing of data in MIM into R}
\description{
  There is an upper limit of about 6000 values (1.231 is one value)
  for the length of the output. If the output is too large (this can
  happen if printing the data set with \code{mim.print("d")}) then the
  last part of the output is not read into mimR. To avoid such
  problems in connection with printing a large dataset use \code{mim.diary.data()}
}
\usage{
mim.diary.data(file = NULL)
}
\arguments{
  \item{file}{Specification of the file the MIM data should be
    written to. When NULL a temporary file is used.}
}

%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}

\value{
A dataframe
}

\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is
  runnning. Unless the dataset in MIM is large, there is no need to use
  mim.diary.data - one might as well use mim.print("d") instead
}
% ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{\code{\link{mim.print}}}

\examples{
data(rats)
mim.read(rats)
d1 <- mim.print("d")

d2 <- mim.diary.data()
}
\keyword{models}
%\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line








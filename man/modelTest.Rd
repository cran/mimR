\name{modelTest}
\alias{modelTest}
\alias{modelTest.mim}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compare two (nested) mim models}
\description{
Compare two nested mim models or test a mim model against saturated model.
}
\usage{
\method{modelTest}{mim}(m1, m2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m1}{mim model object}
  \item{m2}{mim model object}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A list of test statistics
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
}

\references{ David Edwards, An Introduction to Graphical Modelling,
  Second Edition, Springer Verlag, 2000}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

\seealso{ ~~objects to See Also as \code{\link{testdelete}}, ~~~ }
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}% at least one, from doc/KEYWORDS
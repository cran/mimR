\name{mim.cmd}
\alias{mim.cmd}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Submit commands to MIM and get the result back in R}
\description{
  mim.cmd is the core function for communicating with MIM from within R.
  MIM commands are given as text strings and the results from submitting
  these commands in MIM are returned (as a vector of strings). Several
  functions exist for processing these results in sensible ways. See below.
}
\usage{
mim.cmd(mim.cmds, look.nice = TRUE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{mim.cmds}{A (vector of) strings of MIM commands}
  \item{look.nice}{When TRUE the result of the last MIM command is
    returned as a string. When FALSE the result is returned as a vector
    of strings (thereby enabeling processing of the results in R).}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  A vector or a string
  %~Describe the value returned
  %If it is a LIST, use
  %\item{comp1 }{Description of `comp1'}
  %\item{comp2 }{Description of `comp2'}
 % ...
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Second Edition, Springer Verlag, 2000}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{ }

\examples{
mim.cmd(c("fact a2 b2 c2; statread abc",  "25 2 17 8 14 9 6 8 !")) 
mim.cmd("mod ab,ac,bc; fit")
}
\keyword{models}
%\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

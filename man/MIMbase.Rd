\name{MIMbase}
\alias{mim.cmd}
\alias{mcm}
\alias{helpmim}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Submit commands to MIM and get the result back in R}
\description{
  mim.cmd is the core function for communicating with MIM from within R.
  MIM commands are given as text strings and the results from submitting
  these commands in MIM are returned (as a vector of strings). Several
  functions exist for processing these results in sensible ways. See below.

  Invoking mcm() gives a direct interface to MIM such that MIM commands
  can be entered directly. The output printed by MIM is printed the same
  way in the R console.

}
\usage{
mim.cmd(cmd, look.nice = TRUE, return.look.nice=FALSE, version='R')
mcm()
helpmim()
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{cmd}{A (vector of) strings of MIM commands}
  \item{look.nice}{When TRUE the result of the last MIM command is
    printed on the screen and returned as a list of strings, one string
    per line of output. When FALSE the result is returned as a vector
    of strings (thereby enabeling processing of the results in R).}
  \item{return.look.nice}{When TRUE the result of the last MIM command is
    returned as a list of strings, one string
    per line of output.}
  \item{version}{If set to 'S' then the function also should work with Splus}
}
\details{
  To exit the mcm() function, type quit, end, exit, q or e at the
  prompt. Note that this will not terminate the MIM program. The
  helpmim() function invokes the help pages of the MIM program.
}

\value{
  mim.cmd returns a vector or a string, whereas mcm returns NULL
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Second Edition, Springer Verlag, 2000}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{\code{\link{mim}}}

\examples{
mim.cmd("fact a2 b2 c2; statread abc")
mim.cmd("25 2 17 8 14 9 6 8 !")
mim.cmd("mod ab,ac,bc; fit")
##mcm()
}

\keyword{models}
%\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

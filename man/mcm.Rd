\name{mcm}
\alias{mcm}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{MIM command mode}
\description{
  Invoking mcm() gives a direct interface to MIM such that MIM commands
  can be entered directly. The output printed by MIM is printed the same
  way in the R console.
}
\usage{
mcm()
}
%- maybe also `usage' for other objects documented here.
\details{
  To exit the mcm() function, type quit, end, exit, q or e at the
  prompt. Note that this will not terminate the MIM program.
}
\value{
   NULL
 }

\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

\seealso{\code{\link{mim.cmd}}}

\examples{
mcm()
\dontrun{
fact  a2 b2 c2
statread abc 
25 2 17 8 14 9 6 8 !
satmod; fit
print f
}
quit
}
\keyword{models}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

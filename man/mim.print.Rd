\name{mim.print}
\alias{mim.print}
\title{Function for printing data, models, parameter estimates etc. in MIM}
\description{
  Submits the command 'Print <letter>' where the possible letters can be
  looked up in the MIM online help or David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002. The result is interpreted in a suitable way and
  returned to the user
}
\usage{
mim.print(arg = NULL, verbose=FALSE)
}
\arguments{
  \item{arg}{A letter or NULL. When NULL the model is returned}
  \item{verbose}{If TRUE noting is printed}
}
\details{
  For a list of letters (arg) see the only help in MIM. (In MIM, type F1)
}
\value{
  What is returned depend on the arg, i.e. on the letter 
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}
% ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{
  \code{\link{mim.cmd}}
}

\examples{
mim.cmd(c("fact a2 b2 c2; statread abc",  "25 2 17 8 14 9 6 8 !")) 
mim.cmd("mod ab,ac,bc; fit")
mim.print("f")
mim.print("s")
}
\keyword{models}
%\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

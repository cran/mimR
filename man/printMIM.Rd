\name{printMIM}
\alias{printMIM}
\title{Function for printing data, models, parameter estimates etc. in MIM}
\description{
  Submits the command 'Print <letter>' where the possible letters can be
  looked up in the MIM online help or David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002. The result is interpreted in a suitable way and
  returned to the user
}
\usage{
printMIM(arg = NULL, verbose=FALSE)
}
\arguments{
  \item{arg}{A letter or NULL. When NULL the model is returned. See
    'details' below.}
  \item{verbose}{If TRUE noting is printed}
}
\details{
  S:
 empirical counts, means and covariances
 
T:
 empirical counts, linear parameters and precisions
 
U:
 empirical counts, means and correlations
 
V:
 empirical counts, linear parameters and partial correlations
 
F:
 fitted counts, means and covariances
 
G:
 fitted counts, linear parameters and precisions
 
H:
 fitted counts, means and correlations
 
I:
 fitted counts, linear parameters and partial correlations
 
NULL (the default):
 the formula of the current model. In block mode, this is the current block-recursive model, otherwise, it is the current undirected model.
 
M:
 the formula of the current undirected model.
 
B:
 the formula of the current block-recursive model.
 
W:
 The AIC and BIC of the current model.
 
X:
 minus twice the log likelihood of the saturated (or maximum) model
 
Y:
 minus twice the log likelihood of the current model
 
Z:
 the deviance of the current model.
 
C:
 the raw data (using value labels)
 
D:
 the raw data (using levels)
 
E:
 the raw data (showing missing values)
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
mim.cmd("fact a2 b2 c2; statread abc")
mim.cmd("25 2 17 8 14 9 6 8 !") 
mim.cmd("mod ab,ac,bc; fit")
printMIM("f")
printMIM("s")
}
\keyword{models}
%\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

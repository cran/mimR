\name{get.mim.parameters}
\alias{get.mim.parameters}
\title{Function to get the discrete, linear and quadratic parameters of
  MIM models back into R}
\description{
  Function to get the discrete, linear and quadratic parameters of
  MIM models back into R. In R, the parameters are generally organized
  as a list of lists.
}
\usage{
get.mim.parameters(mim.output)
}
\arguments{
  \item{mim.output}{The output from submitting the command
    mim.cmd("print <letter>"), where letter can be s,t,u,v,f,g,h,i}
}
\details{
  This function is used by the
  mim.print function, and the user need not use get.mim.parameters().
  Examples of the usage is given below.
}
\value{
  An object of type mim.parameters, which in general is a list of lists
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}
% ~Make other sections like WARNING with \section{WARNING }{....} ~

%~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{\code{\link{mim.print}}}
\examples{
data(rats)
mim.read(rats, mim.names=c("a","b","x","y"), mim.labels=names(rats))
mim.cmd("model ab/abx,aby/x,y; fit")

r1 <- mim.cmd("print t")
r2 <- get.mim.parameters(r1)
r2
## equivalent to 
mim.print("t")

mim.cmd(c("fact a2 b2 c2; statread abc",  "25 2 17 8 14 9 6 8 !")) 
mim.cmd("mod ab,ac,bc; fit")

s1 <- mim.cmd("print f")
get.mim.parameters(s1)
## equivalent to 
mim.print("f")
}
\keyword{models}
%\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

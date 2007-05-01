\name{MIMmodels}

\alias{mim}
\alias{chainmim}
\alias{DF}
\alias{DF.mim}
\alias{deviance.mim}
\alias{likelihood}


%- Also NEED an `\alias' for EACH other topic documented here.
\title{Create undirected MIM models }
\description{
  Create undirected MIM models 
}

\usage{
mim(mimFormula, data, fit=TRUE, marginal=data$name)
%chainmim(mimFormula.list, data)
%mim.br.model(mim.formula, data, mim.names = TRUE, submit = TRUE, fit=TRUE)

}

\arguments{
  \item{mimFormula}{A model formula following the MIM syntax. Long
    variable names are allowed however. See 'details'. The formula can
    be given either with a tilde or as a string}
  \item{fit}{Should the model be fitted if possible}
  \item{data}{A gmData object}
  \item{marginal}{Can be used for specifying only a subset of the
    variables in connection with a main effects, a saturated and a
    homogeneous saturated model}
} 
\details{
  A mim.formula can be
  "Sex+Drug/Sex:W1+Drug:W1+Sex:W2+Drug:W2/Sex:W1:W2+Drug:W1:W2".
  A mimFormula can also be "." (the main effects (the
  independence) model), ".." (the saturated model) or "..h"
  (the homogeneous saturated model).
  See 'examples'.
}
\value{
  A mim model object
}
\references{ David Edwards, An Introduction to Graphical Modelling,
  Springer Verlag, 2002}
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{Before using mimR, make sure that the MIM program is runnning.}

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{\code{\link{as.gmData}}}

\examples{
# Create som models (no data needed!)
gmd  <-  gmData(c("Sex","Drug","W1","W2"),
    factor=c(2,3,FALSE,FALSE),
    vallabels=list("Sex"=c("M","F"), "Drug"=c("D1","D2","D3")))


m12   <- mim("Sex:Drug/Sex:Drug:W1+Sex:Drug:W2/W1:W2", data=gmd)

summary(m12)

m.main <- mim(".",  data=gmd)
m.sat  <- mim("..",  data=gmd)
m.hsat <- mim("..h", data=gmd)

summary(m.main); 
summary(m.sat); 
summary(m.hsat)

# Next we need some data to work with
data(rats)
gmd.rats <- as.gmData(rats)
vallabels(gmd.rats)
observations(gmd.rats)

m1   <- mim("Sex:Drug/Sex:Drug:W1+Sex:Drug:W2/W1:W2", data=gmd.rats)

m.main <- mim(".",   data=gmd.rats, marginal=c("Sex", "Drug", "W1"))
m.sat  <- mim("..",  data=gmd.rats, marginal=c("Sex", "Drug", "W1"))
m.hsat <- mim("..h", data=gmd.rats, marginal=c("Sex", "Drug", "W1"))

m1f  <- fit(m1)


summary(m1f)

m.main <- fit(mim(".",  data=gmd.rats))
m.sat  <- fit(mim("..",  data=gmd.rats))
m.hsat <- fit(mim("..h", data=gmd.rats))

summary(m.main); 
summary(m.sat); 
summary(m.hsat)

# To generate an nth order hierarchical log-linear model for discrete
# data you can do

data(HairEyeColor)
mim(nthOrderModel(names(dimnames(HairEyeColor)),order=2),data=as.gmData(HairEyeColor))

}
\keyword{models}% at least one, from doc/KEYWORDS

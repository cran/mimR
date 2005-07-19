\name{hiv}
\alias{hiv}
\docType{data}
\title{HIV}
\description{
The data consists of 6 blood measurements taken on 107 babies:
  \itemize{
    \item immunoglobin G,
    \item immunoglobin A,
    \item lymphocyte B,
    \item platelet count,
    \item lymphocyte T4,
    \item T4/T8 lymphocyte ratio.
  }
}
\usage{data(hiv)}
\format{
  A list providing a summary of the data. The list contains:
  \tabular{lcl}{
    \code{hiv$means} \tab : \tab the mean vector,\cr 
    \code{hiv$stddev}   \tab : \tab the vector of standard deviations,\cr
    \code{hiv$corr} \tab : \tab the correlation matrix, and \cr
    \code{hiv$n}    \tab : \tab the sample size.
  }
}
\source{
  Roverato, A. \& Whittaker, J.  (1996) A hyper normal prior distribution
  for approximate Bayes factor calculations on non-decomposable
  graphical Gaussian models, unpublished manuscript.
}
\examples{
data(hiv)
hiv$means
hiv$stddev
hiv$corr
hiv$n

v <- as.gmData(hiv)
m <- mim("..",data=v)
summary(fit(m))
}
\keyword{datasets}

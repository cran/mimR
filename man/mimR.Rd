\name{mimR}
\alias{mimR}
\title{
The package `mimR': summary information
}
\description{
This package provides an interface to the MIM program for inference in
mixed graphical models.
}
\section{Details}{
\itemize{
  \item mimR is available on Windows platforms only.
  \item mimR requires that the MIM program is installed on the computer.
  \item MIM can be downloaded from http://www.hypergraph.dk.
  \item mimR requires that the R package 'RDCOMClient' is installed.
  \item For information about required versions of R and MIM,
  please see the information presented when mimR is loaded.

  \item mimR will automatically start the MIM program if not already running. 
  However, mimR sometimes runs more smoothly if the user starts MIM manually.

  \item The mimR package comes with a small users manual.
  
  \item To start using mimR you must do
  library(mimR)
  }
The package is intended as a contribution to the gR-project 
derscribed by Lauritzen (2002).
}
\section{Authors}{
Søren Højsgaard, Biometry Research Unit, Danish Institute of
Agricultural Sciences, DK-8830 Tjele, Denmark
}
\section{Acknowledgements}{
  Thanks to David Edwards, the creator of the MIM program.
}
\references{
Lauritzen, S. L. (2002). gRaphical Models in R.
\emph{R News}, 3(2)39.
}
\keyword{multivariate}
\keyword{models}
\keyword{graphs}

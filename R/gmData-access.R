
### Some generic functions
### BEGIN(EXPORT)
"latent.gmData" <- function(x){attr(x,"latent")}
"latent.mim"    <- function(x){.latentInModel(x)}
"latent" <- function(x) UseMethod("latent")

"latent<-.gmData" <- function(tmp,value){attr(tmp,"latent")<-value; return(tmp)}
"latent<-" <- function(tmp,value) UseMethod("latent<-")

vallabels.gmData<- function(x) attr(x,"vallabels")
vallabels       <- function(x) UseMethod("vallabels")

"vallabels<-.gmData"<- function(tmp,value){attr(tmp,"vallabels")<-value; return(tmp)}
"vallabels<-"       <- function(tmp,value) UseMethod("vallabels")

observations.gmData <- function(x) attr(x,"observations")
observations    <- function(x) UseMethod("observations")
obs             <- function(x) UseMethod("observations")

"observations<-.gmData"<- function(tmp,value){attr(tmp,"observations")<-value; return(tmp)}
"observations<-"       <- function(tmp,value)UseMethod("observations<-")

"description.gmData" <- function(x){attr(x,"description")}
"description" <- function(x) UseMethod("description")

"description<-.gmData" <- function(tmp,value){attr(tmp,"description")<-value; return(tmp)}
"description<-" <- function(tmp,value) UseMethod("description<-")

.dataOrigin   <- function(x) attr(x,"dataOrigin")
### END(EXPORT)

### Some generic functions

"latent.gmData" <- function(x){attr(x,"latent")}
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


gmData <- function(name, letter=c(letters,LETTERS), factor=rep(FALSE,length(name)),
                   vallabels=NULL, data=NULL){
  value <- as.data.frame(cbind(name, letter[1:length(name)]))
  fac   <- factor!=0    
  lev   <- factor
  lev[lev==0]  <- NA
  names(value) <- c("name","letter")
  value$factor <- fac;
  value$levels <- lev
  class(value) <- c("gmData","data.frame")

  attr(value,"vallabels")      <- vallabels
  attr(value,"observations")   <- data
  switch(class(data),
         "table"=     { attr(value,"dataOrigin")     <- "table"      },
         "data.frame"={ attr(value,"dataOrigin")     <- "data.frame" },
         NULL=        { attr(value,"dataOrigin")     <- "table"      })
  return(value)
}


##################################################################################
as.gmData       <- function(data,letter=c(letters,LETTERS)) UseMethod("as.gmData")
##################################################################################
as.gmData.data.frame <- function(data,letter=c(letters,LETTERS)){
  nt <- .extract.nt(data,letter)
  attr(nt,"dataOrigin"  )   <- "data.frame"
  attr(nt,"observations")   <- data
  class(nt) <- c("gmData","data.frame")
  return(nt)
  }

as.gmData.table <- function(data,letter=c(letters,LETTERS)){
  counts <- as.vector(data)
  dn     <- dimnames(data)
  name   <- names(lapply(dn,function(x)names(x)))
  dim    <- unlist(lapply(dn,length))
  nt     <- gmData(name,factor=dim)
  attr(nt,"dataOrigin")     <- "table"
  attr(nt,"observations")   <- data
  class(nt) <- c("gmData","data.frame")
  return(nt)
  }


print.gmData        <- function(x, ...){
  if (!is.null(description(x)))
    cat("Description:", description(x), "\n")
  print.data.frame(x);
  cat("Data origin:     ", .dataOrigin(x),"\n")
  if (!is.null(latent(x)))
    cat ("Latent variables:", paste(latent(x),collapse=' '), "\n")
  cat("To see the values of the factors use the 'vallabels' function\n")
  cat("To see the data use the 'observations' function\n")
  return(x)
}

.nt.as.gmData <- function(nt){
  attr(nt,"dataOrigin")  <- "no.data"
  attr(nt,"observations")   <- NULL
  class(nt) <- c("gmData","data.frame")
  return(nt)
}

.extract.nt <- function(data,letter=c(letters,LETTERS)) UseMethod(".extract.nt")

.extract.nt.data.frame <- function(data,letter=c(letters,LETTERS)){    
  name   <- names(data)  
  fact   <- unlist(lapply(1:ncol(data), function(j) is.factor(data[,j])))
  levels <- unlist(lapply(1:ncol(data),
                          function(j){if(is.factor(data[,j])) length(levels(data[,j])) else NA}))

  value     <- data.frame(name, letter[1:length(name)], fact, levels)
  names(value) <- c("name", "letter", "factor", "levels")

  if (length(which(fact))>0){
    vallabels <- NULL
    for (j in which(fact)){
      vallabels <- c(vallabels, list(levels(data[,j])))
    }
    names(vallabels) <- names(data[which(fact)])
    ##print(vallabels)
  } else {
    vallabels <- NULL
  }
  attr(value,"vallabels") <- vallabels
  class(value) <- c("gmData","data.frame")
  return(value)
}


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

print.gmData        <- function(x, ...){
  if (!is.null(description(x)))
    cat("Description:", description(x), "\n")
  print.data.frame(x);
  cat("Data origin:     ", .dataOrigin(x),"\n")
  if (!is.null(latent(x)))
    cat ("Latent variables:", paste(latent(x),collapse=' '), "\n")
  cat("To see the values of the factors use the 'vallabels' function\n")
  cat("To see the data use the 'observations' function\n")
  #print(class(x))
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


gmData <- function(name, letter=c(letters,LETTERS), factor=rep(FALSE,length(name)),
                   vallabels=NULL, data=NULL){
  value <- as.data.frame(cbind(name, letter[1:length(name)]))
  fac <- factor!=0    
  lev <- factor
  lev[lev==0] <- NA
  names(value) <- c("name","letter")
  value$factor <- fac;
  value$levels <- lev
  class(value) <- c("gmData","data.frame")

  attr(value,"vallabels") <- vallabels
  attr(value,"dataOrigin")     <- "no.data"
  attr(value,"observations")   <- data
  return(value)
}


as.mimData      <- function(data,letter=c(letters,LETTERS))
  as.gmData.data.frame(data, letter)

##################################################################################
as.gmData       <- function(data,letter=c(letters,LETTERS)) UseMethod("as.gmData")
##################################################################################
as.gmData.data.frame <- function(data,letter=c(letters,LETTERS)){
  nt <- .extract.nt(data,letter)
  attr(nt,"dataOrigin")  <- "data.frame"
  attr(nt,"observations")   <- data
  class(nt) <- c("gmData","data.frame")
  return(nt)
  }

as.gmData.table <- function(data,letter=c(letters,LETTERS)){
  counts <- as.vector(data)
  dn <- dimnames(data)
  name<-names(lapply(dn,function(x)names(x)))
  dim <-unlist(lapply(dn,length))
  nt <- gmData(name,factor=dim)
  attr(nt,"dataOrigin")  <- "table"
  attr(nt,"observations")   <- data
  class(nt) <- c("gmData","data.frame")
  return(nt)
  }

as.gmData.suffStats <- function(data, letter=c(letters,LETTERS)){

  nt <- data$Variables
  q.by <- .q.by(data)
  l.by <- .l.by(data)
  d.by <- .d.by(data)

  d.names <- .d.names(data)
  d.levels<- .d.levels(data)
  c.names <- .c.names(data)

  res <- NULL
  if ( length(d.by) > 1 ) {
    for (i in 1:length(d.by)){
      if (data$homogeneous==TRUE){
        curr.q <- q.by
      } else {
        curr.q <- q.by[[i]]
      }
      covm <- matrix(unlist(curr.q),ncol=length(l.by[[i]]));
      covm.tri <- t(covm)[!lower.tri(covm)];
      res <- rbind(res, c(d.by[[i]], l.by[[i]], covm.tri))
    }
  } else  {
    covm <- matrix(q.by[[1]],ncol=length(c.names))
    covm.tri <- t(covm)[!lower.tri(covm)]
    res <- rbind(res, c(as.numeric(d.by), unlist(l.by), unlist(covm.tri)))
  }

  attr(nt,"dataOrigin")     <- "suffStats"
  attr(nt,"observations")   <- res
  class(nt) <- c("gmData","data.frame")
  return(nt)
}



#res <- mim.cmd( paste("print ", "d"), look.nice=FALSE,return.look.nice=FALSE)
#names(datm) <- msim1$name

retrieveData <- function(gmData=NULL,impute=FALSE){
  if (impute==TRUE)
    mim.cmd("impute")
  value<- printMIM("d")
  dat <-  value[-(1:(length(gmData$name)+1))]
  datm<-matrix(.silent.as.numeric(dat), ncol=length(gmData$name)+1,byrow=TRUE)  
  datm<-as.data.frame(datm[,-1])
  #if (!is.null(gmData))
    names(datm) <- gmData$name
  return(datm)
}




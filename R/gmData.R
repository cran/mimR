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

### NEW
# getData               <- function(x) UseMethod("observations")
# getLabels             <- function(x) UseMethod("vallabels")
#

gmData <- function(name, letter=NULL,
                   factor=rep(FALSE,length(name)),
                   vallabels=NULL, data=NULL){
  if (is.null(letter))
    letter <- c(letters,LETTERS)
  value <- as.data.frame(cbind(name, letter[1:length(name)]))
  fac   <- factor!=0    
  lev   <- factor
  lev[lev==0]  <- NA
  names(value) <- c("name","letter")
  value$factor <- fac;
  value$levels <- lev

  factor.index <- which(!is.na(value$levels))
  if (length(factor.index)>0){
    vl <- NULL
    for (j in factor.index){
      if (!is.null(vallabels)){
        if (factor[j]!=length(vallabels[[j]])){
          cat("Error: Factor",name[j], "has", factor[j],"levels, but",
              length(vallabels[[j]]), "labels have been specified\n These are",
              vallabels[[j]],"\n")
          stop("Exiting", call.=FALSE)
        }
        vl <- c(vl, list(vallabels[[j]]))
      } else {
        vl <- c(vl, list(1:factor[j]))
      }
    }
  names(vl) <- name[factor.index]
  }

  value$name   <- as.character(value$name)
  value$letter <- as.character(value$letter)
  class(value) <- c("gmData","data.frame")
  
  attr(value,"vallabels")      <- vallabels
  attr(value,"observations")   <- data
  switch(class(data),
         "table"=     { attr(value,"dataOrigin")     <- "table"      },
         "data.frame"={ attr(value,"dataOrigin")     <- "data.frame" },
         NULL=        { attr(value,"dataOrigin")     <- "table"      })


  return(value)
}


.extract.nt <- function(data,letter=NULL) UseMethod(".extract.nt")

.extract.nt.data.frame <- function(data,letter=NULL){
  name   <- names(data)
  if (is.null(letter)){
    if ( all(nchar(name)==1) )
      letter<-substr(n,1,1)
    else
      letter<- c(letters,LETTERS)
  } else {
    if (length(unique(letter))!=length(name))
      stop("Number of letters do no match the number of variables\n or there might be dublicates among the letters",call.=FALSE)
  }
  
  fact   <- unlist(lapply(1:ncol(data), function(j) is.factor(data[,j])))
  levels <- unlist(lapply(1:ncol(data),
                          function(j){if(is.factor(data[,j]))
                                        length(levels(data[,j])) else NA}))

  value     <- data.frame(name, letter[1:length(name)], fact, levels)

  names(value) <- c("name", "letter", "factor", "levels")


  if (length(which(fact))>0){
    vallabels <- NULL
    for (j in which(fact)){
      vallabels <- c(vallabels, list(levels(data[,j])))
    }
    names(vallabels) <- names(data[which(fact)])
  } else {
    vallabels <- NULL
  }
  attr(value,"vallabels") <- vallabels

  value$name   <- as.character(value$name)
  value$letter <- as.character(value$letter)
  
  class(value) <- c("gmData","data.frame")
  return(value)
}

##################################################################################
as.gmData       <- function(data,letter=NULL) UseMethod("as.gmData")
##################################################################################


as.gmData.data.frame <- function(data,letter=NULL){
  nt <- .extract.nt(data,letter)
  attr(nt,"dataOrigin"  )   <- "data.frame"
  attr(nt,"observations")   <- data
  class(nt) <- c("gmData","data.frame")
  return(nt)
  }

as.gmData.table <- function(data,letter=NULL){
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


as.gmData.list <- function(data,letter=NULL){
  is.cont <- !any(is.na(match(names(data),c("names","means","n","corr","stddev"))))
  is.disc <- !any(is.na(match(names(data),c("names","levels","counts","vallabels"))))
  if (is.cont){
    if (!is.null(data$names)){
      name <- data$names
    } else {
      if (!is.null(names(data$means))){
        name <- names(data$means)
        data$names <- name
      } else {
        stop("Names can not be found")
      }
    }
    value <- gmData(names(data$means))
    attr(value, "dataOrigin")   <- "contSuffStats"
    
  }
  if (is.disc){
    value <- gmData(data$names, factor=data$levels,vallabels=data$vallabels)
    attr(value, "dataOrigin")   <- "discSuffStats"
  }
  attr(value, "observations") <- data
  return(value)
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



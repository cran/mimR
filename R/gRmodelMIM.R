
.is.fitted            <- function(x) ifelse (is.null(x$modelInfo), FALSE, x$modelInfo$Fitted)
.mimFormula           <- function(x) x$mimFormula
.mimFormula.letter    <- function(x) x$mimFormula.letter
.Formula.as.string    <- function(x) x$modelInfo$Formula.as.string
.DF                   <- function(x) x$modelInfo$DF
.likelihood           <- function(x) x$modelInfo$likelihood
.cliques              <- function(x) x$modelInfo$Cliques

DF.mim                <- function(x) x$modelInfo$DF
DF                    <- function(x) UseMethod("DF")

likelihood            <- function(x) x$modelInfo$likelihood
cliques               <- function(x) x$modelInfo$Cliques
deviance.mim          <- function(object, ...) object$deviance



mim <- function(mimFormula, data, letter=FALSE, marginal=data$name){
  mimStringFormula <- mimFormula 
  if (class(mimStringFormula)=="formula"){
    stop(paste("Model formulae '~...' not allowed in mimR\n",
               "Specify the model as a string, following the MIM syntax\n"),
         call.=FALSE)
  }

  mim.cmd("clear; clear output")
  vs  <- .nt.to.varspec (data)
  lapply(vs, function(s){if(!is.null(s)) mim.cmd(s)})
  
  if( !is.na(match(mimStringFormula, c("..", "..h", ".")))){
    marg <- if (letter==FALSE)
      .namesToLetters(marginal, data)
    else 
      marginal

    model.type <-switch(mimStringFormula,
                        ".."  = {"SatMod"   },
                        "..h" = {"HomSatMod"},
                        "."   = {"Main"     })
    
    mim.cmd(paste(model.type, paste(marg,collapse=' ')))
    rsm <- .RSmodel();    
    mimFormula.letter <- rsm$Formula.as.string
    value <-.make.mim(mimFormula.letter, data, letter=TRUE, rsm=rsm);
  } else {
    tmp   <- .make.mim(mimStringFormula, data, letter=letter)
    rsm   <- .RSmodel(tmp$mimFormula.letter)
    value <- .make.mim(mimStringFormula, data, letter=letter,rsm=rsm)
  }
  mim.cmd(paste("# model ", value$mimFormula))
  mim.cmd(paste("model ", value$mimFormula.letter))
  return(value)
}

.make.mim <- function(mimStringFormula, data, letter=FALSE, rsm=NULL,rsp=NULL){
  ### DOES NOT CALL MIM

  parsedFormula     <- .parseMimStringFormula(mimStringFormula,letter=letter)
  parsedFormula2    <- .swapNamesLetters(parsedFormula,data)
  mimStringFormula2 <- .parsedMimFormula2mimStringFormula(parsedFormula2)
  if (letter==TRUE){
    mimFormula.letter <- mimStringFormula
    mimFormula.name   <- mimStringFormula2
  } else {
    mimFormula.name   <- mimStringFormula
    mimFormula.letter <- mimStringFormula2
  }
  value <-list("mimFormula"        =mimFormula.name,
               "mimFormula.letter" =mimFormula.letter,
               "modelInfo"         =rsm,
               "data"              =data)
  if (!is.null(rsp))
    if (!is.na(rsp) && !is.na(rsm)){
      rsp$Variables <- rsm$Variables
      value$suffStats <- rsp
    }
  value$used.names <- .used.names(value)
  class(value) <- "mim"
  return(value) 
}


.parseMimStringFormula <- function(mimFormula, letter=FALSE){
  # Turns a formula (as a string) into a list with discrete, linear and quadratic generators
  s1 <- unlist(strsplit(mimFormula, "/"))
  if (length(s1)==2)    s1 <- s1[1]
  s1 <- c(s1, NA, NA)[1:3]

  if (letter==FALSE){
    s3 <-lapply(s1, function(s){
      if(is.na(s))
        return(s)
      else {
        s2 <- strsplit(s, "\\+")    
        s2 <- unlist(lapply(s2, function(x)gsub(" +","",x)))
        s3 <- strsplit(s2,":")
        return(s3)
      }
    })
    
  } else {
    s3 <-lapply(s1, function(s){
      if(is.na(s))
        return(s)
      else {
        s2 <- strsplit(s, "[+,]")    
        s2 <- unlist(lapply(s2, function(x)gsub(" +","",x)))
        s3 <- strsplit(s2,"")
        return(s3)
      }
    })
  }
  names(s3) <- c("discrete", "linear", "quadratic")
  s3$letter <- letter
  return(s3)        
}

.swapNamesLetters <- function(parsedMimFormula, nt){
  if (parsedMimFormula$letter==TRUE){
    value<-lapply(parsedMimFormula[1:3], lapply, .lettersToNames, nt)
    value$letter <- FALSE
  } else {
    value<-lapply(parsedMimFormula[1:3], lapply, .namesToLetters, nt)
    value$letter <- TRUE    
  }
  return(value)
}

.parsedMimFormula2mimStringFormula <- function(parsedMimFormula){

  isna <- which(unlist((lapply(lapply(parsedMimFormula, is.na),all))))
  for (i in isna)
    parsedMimFormula[[i]]<-''
  
  if (parsedMimFormula$letter==TRUE){
    value<-lapply(parsedMimFormula[1:3], lapply, paste, collapse='')
    value<-lapply (lapply(value, unlist), paste,collapse=',')
    value<-paste(unlist(value),collapse='/')
  } else {
    value<-lapply(parsedMimFormula[1:3], lapply, paste, collapse=':')
    value<-lapply (lapply(value, unlist), paste,collapse='+')
    value<-paste(unlist(value),collapse='/')
  }
  return(value)
}



retrieveMIMvalues <- function(data=NULL){
  rsm <- .RSmodel()
  rsp <- .RSprint.suffStats()
  if (is.na(rsm[1])){
    return(NULL)
  } else { 
    if (is.null(data)){
      if (!is.na(rsp[1])){
        rsp$Variables <- rsm$Variables
        print(rsp)
        data <- as.gmData(rsp)
      } else {
        data <- .nt.as.gmData(rsm$Variables)
      }
    }    
    mimFormula.letter <- rsm$Formula.as.string
    x1 <- .parseMimStringFormula(mimFormula.letter,letter=TRUE)
    mimFormula.name <- .parsedMimFormula2mimStringFormula(.swapNamesLetters(x1, data))
    value <- .make.mim(mimFormula.letter, data, letter=TRUE, rsm=rsm,rsp=rsp)
    return(value)
  }
}

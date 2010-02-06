
fit.mim <- function(object, arg=NULL, ...){
  tryfit <- function(arg){
    emAttempts <- 0
    repeat{
      if (!is.null(.latentInModel(object))){
        .initLatent (.latentInModel(object), .getgmData(object))
        if(!is.na(match("r", unlist(strsplit(arg,"")))))
          arg2 <- 'er'
        else
          arg2 <- 'es'
      } else {
        arg2 <- arg
      }
      v <- .RSfit(arg2);  

      emAttempts <- emAttempts + 1
      if (!is.null(v[1]) || emAttempts>20)
        break()
    }
    return(v)
  } #-------------
  
  if (is.null(arg))
    arg <- ""

  EMrequested <- !is.na(match("e", unlist(strsplit(arg,""))))
  
  if (!EMrequested && !is.null(.latentInModel(object))){
    cat("Model has latent variable - trying EM algorithm\n")
    arg<-gsub(" ","",paste(arg, "e"))
  }
  toMIM(.getgmData(object))

  mim.cmd(paste("# Model", object))
  
  str  <- paste("Model ", mimFormulaLetters(object))
  str2 <- .str2strlist(str)
  lapply(str2, mim.cmd)

  ##mim.cmd(paste("Model", mimFormulaLetters(object)))

  v <- tryfit(arg)

  if (!EMrequested){
    if (is.na(v[1])){
      cat("Seems that there are incomplete observations - trying EMfit\n")
      v <- tryfit(paste(arg, "e"))
      if (is.null(v[1]) || is.na(v[1])){
        cat("... Fitting failed...\n")
        return ()
      } 
    }  
  } else {
    if (is.null(v[1]) || is.na(v[1])){
      cat("... Fitting failed...\n")
      return()
    }
  }

                                        #print(".retrieve.fittedMIM")
  object <- .retrieve.fittedMIM(object)
  return(object)
}



## fit.mim <- function(m, arg=NULL, ...){
##   mim <- m
##   tryfit <- function(arg){
##     emAttempts <- 0
##     repeat{
##       if (!is.null(.latentInModel(mim))){
##         .initLatent (.latentInModel(mim), .getgmData(mim))
##         if(!is.na(match("r", unlist(strsplit(arg,"")))))
##           arg2 <- 'er'
##         else
##           arg2 <- 'es'
##       } else {
##         arg2 <- arg
##       }
##       v <- .RSfit(arg2);  

##       emAttempts <- emAttempts + 1
##       if (!is.null(v[1]) || emAttempts>20)
##         break()
##     }
##     return(v)
##   } #-------------
  
##   if (is.null(arg))
##     arg <- ""

##   EMrequested <- !is.na(match("e", unlist(strsplit(arg,""))))
  
##   if (!EMrequested && !is.null(.latentInModel(mim))){
##     cat("Model has latent variable - trying EM algorithm\n")
##     arg<-gsub(" ","",paste(arg, "e"))
##   }
##   toMIM(.getgmData(mim))

##   mim.cmd(paste("# Model", mim))
  
##   str  <- paste("Model ", mimFormulaLetters(mim))
##   str2 <- .str2strlist(str)
##   lapply(str2, mim.cmd)

##   ##mim.cmd(paste("Model", mimFormulaLetters(mim)))

##   v <- tryfit(arg)

##   if (!EMrequested){
##     if (is.na(v[1])){
##       cat("Seems that there are incomplete observations - trying EMfit\n")
##       v <- tryfit(paste(arg, "e"))
##       if (is.null(v[1]) || is.na(v[1])){
##         cat("... Fitting failed...\n")
##         return ()
##       } 
##     }  
##   } else {
##     if (is.null(v[1]) || is.na(v[1])){
##       cat("... Fitting failed...\n")
##       return()
##     }
##   }

##                                         #print(".retrieve.fittedMIM")
##   mim <- .retrieve.fittedMIM(mim)
##   return(mim)
## }



### .functions below here ####

.retrieve.fittedMIM <- function(object){
  mim <- object
  v1<- .RSmodel()
  v2<- .RSprint.suffStats()
  v2$Variables      <- v1$Variables
  mim$modelInfo     <- v1;
  mim$suffStats     <- v2;
  return(mim)
}

.initLatent <- function(set, data, info=FALSE){
    s <- names2letters(set, data)
    if (info==TRUE)
      cat("Initializing latent variables:", paste(set, collapse=' '), "\n")
    s2<- paste("calc", s, "=", s, "+ln(0);")
    mim.cmd(s2)
}




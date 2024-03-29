.parseMIMstructure <- function(s){
  mims <- paste(s,collapse='');
  
  if (length(grep("Insufficient", s)))
    return(NA)

                                        # print(s)
  mims <- gsub(" ","",mims)
  mims <- gsub(",)",")",mims)
  mims <- eval(parse(text=mims))
                                        # print(mims)
  return(mims)
}

.RSoptions <- function(){
  mimobj <- mim.cmd("RSoptions")
  value <- .parseMIMstructure(mimobj)
  return(value)  
}

.RSfit  <- function(arg=NULL){
  str <- paste("RSfit ", arg)
  ##s <<- str
  mimobj <- mim.cmd(str, look.nice=FALSE, return.look.nice=TRUE)
  value <- .parseMIMstructure(mimobj)
  return(value)  
}


.RSprint <- function(arg="f"){
  mim.cmd("pf 12,7", look.nice=FALSE)
  mimobj <- mim.cmd(paste("RSprint ",arg), look.nice=FALSE, return.look.nice=TRUE)
  value <- .parseMIMstructure(mimobj)
  class(value)<-"stats"
  return(value)
}

.RSprint.suffStats <- function(){
  value <- .RSprint("s")
  class(value) <- "suffStats"
  return(value)
}



.RSmodel <- function(arg=NULL){

  ##print(".RSmodel")
  mim.cmd(paste("# .RSmodel", arg))
  mim.cmd("pf 15,9", look.nice=FALSE)
  mimobj <- mim.cmd(paste("RSmodel ",arg), look.nice=FALSE, return.look.nice=TRUE)

                                        #print(mimobj)
  if (length(grep("Error",mimobj))>0)
    stop("The model formula is invalid ", call.=FALSE)
  value <- .parseMIMstructure(mimobj)

                                        # print(value)
  
  if (is.null(value) || is.na(value)){
    cat("MIM returned NULL or NA, can not continue")
    return()
  }

  class(value$FittedValues) <- "FittedValues" 
  if (!is.na(value[1]))
    class(value)<-"modelInfo"    
  
  value$Variables$name   <- as.character(value$Variables$name)
  value$Variables$letter <- as.character(value$Variables$letter)

  Formula.as.string    <- value$Formula.as.string
  Formula.as.list      <- value$Formula.as.list
  mimFormula.as.list   <- src2tgt(Formula.as.list,
                                  src=value$Variables$letter,
                                  tgt=value$Variable$name)


  mimFormula.as.string <- list2stringNames(mimFormula.as.list)
  Delta    <- value$Delta
  Gamma    <- value$Gamma
  mimDelta <- src2tgt(Delta,
                      src=value$Variables$letter,
                      tgt=value$Variable$name)
  mimGamma <- src2tgt(Gamma,
                      src=value$Variables$letter,
                      tgt=value$Variable$name)

  value$Formula.as.string   <- value$Formula.as.list<- value$Delta<- value$Gamma <-NULL

  value$Formula.as.string   <- Formula.as.string
  value$Formula.as.list     <- Formula.as.list
  value$mimFormula.as.string<- mimFormula.as.string
  value$mimFormula.as.list  <- mimFormula.as.list
  value$Delta    <- Delta
  value$Gamma    <- Gamma
  value$mimDelta <- mimDelta
  value$mimGamma <- mimGamma
  return(value)
}

.RSoptions <- function(){
  mimobj <- mim.cmd("RSoptions",  look.nice=FALSE, return.look.nice=TRUE)
  value <- .parseMIMstructure(mimobj)
  return(value)
}

.RStestdelete <- function(edge,options=NULL){
  mimobj <- mim.cmd(paste("RStestdelete ", paste(edge, collapse=''),options),
               look.nice=FALSE, return.look.nice=TRUE)
  ## print(".RStestdelete -- mimobj:::"); print(mimobj)
  value <- .parseMIMstructure(mimobj);
  return(value)
}



.RStest  <- function(){
  mimobj <- mim.cmd(paste("RStest "), look.nice=FALSE, return.look.nice=TRUE)
  value <- .parseMIMstructure(mimobj)
  return(value)  
}

.rsprint <- function(arg="f"){
  fvobj <- .mim.cmd.term(paste("RSprint ",arg),
                         look.nice=FALSE, return.look.nice=TRUE)
  fvobj <- paste(fvobj,collapse='');
  fvobj <- eval(parse(text=fvobj))
  class(fvobj)<-"stats"
  return(fvobj)
}

.rsprint.suffStats <- function(){
  value <- .rsprint("s")
  class(value) <- "suffStats"
  return(value)
}

.rsmodel <- function(arg=NULL){
  fvobj <- .mim.cmd.term(paste("RSmodel ",arg),
                         look.nice=FALSE, return.look.nice=TRUE)
  fvobj <- paste(fvobj,collapse='');
  value <- eval(parse(text=fvobj))

  v <- value$FittedValues
  class(v) <- "FittedValues"
  value$FittedValues <- v
  if (!is.na(value[1]))
    class(value)<-"modelInfo"    
  return(value)
}

.rsoptions <- function(){
  fvobj <- .mim.cmd.term("RSoptions",
                         look.nice=FALSE, return.look.nice=TRUE)
  fvobj <- paste(fvobj,collapse='');
  value <- eval(parse(text=fvobj))
  return(value)
}






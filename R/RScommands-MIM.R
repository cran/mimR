.RSprint <- function(arg="f"){
  mim.cmd("pf 12,8",look.nice=FALSE)
  fvobj <- .mim.cmd.term(paste("RSprint ",arg),
                         look.nice=FALSE, return.look.nice=TRUE)
  fvobj <- paste(fvobj,collapse='');
  fvobj <- eval(parse(text=fvobj))
  class(fvobj)<-"stats"
  return(fvobj)
}

.RSprint.suffStats <- function(){
  value <- .RSprint("s")
  class(value) <- "suffStats"
  return(value)
}

.RStestdelete <- function(edge,options=NULL){
  v <- mim.cmd(paste("RStestdelete ", paste(edge, collapse=''),options),
               look.nice=FALSE)
  v <- paste(v,collapse='');
  v <- eval(parse(text=v))
  return(v)
}

.RSmodel <- function(arg=NULL){
  fvobj <- .mim.cmd.term(paste("RSmodel ",arg),
                         look.nice=FALSE, return.look.nice=TRUE)
  if (length(grep("Error",fvobj))>0)
    stop("The model formula is invalid ", call.=FALSE)
  fvobj <- paste(fvobj,collapse='');
  value <- eval(parse(text=fvobj))

  v <- value$FittedValues
  class(v) <- "FittedValues"
  value$FittedValues <- v
  if (!is.na(value[1]))
    class(value)<-"modelInfo"    
  return(value)
}

.RSoptions <- function(){
  fvobj <- .mim.cmd.term("RSoptions",
                         look.nice=FALSE, return.look.nice=TRUE)
  fvobj <- paste(fvobj,collapse='');
  value <- eval(parse(text=fvobj))
  return(value)
}






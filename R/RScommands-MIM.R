.parseMIMstructure <- function(s){
  mims <- paste(s,collapse='');
  mims <- eval(parse(text=mims))
  return(mims)
}

.RSprint <- function(arg="f"){
  mim.cmd("pf 12,8")
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
  mimobj <- mim.cmd(paste("RSmodel ",arg), look.nice=FALSE, return.look.nice=TRUE)
  if (length(grep("Error",mimobj))>0)
    stop("The model formula is invalid ", call.=FALSE)
  value <- .parseMIMstructure(mimobj)
  class(value$FittedValues) <- "FittedValues"
  if (!is.na(value[1]))
    class(value)<-"modelInfo"    
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
  value <- .parseMIMstructure(mimobj);
  return(value)
}


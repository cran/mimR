.rsprint <- function(arg="f"){
  fvobj <- mim.cmd(paste("RSprint ",arg), look.nice=FALSE, return.look.nice=TRUE)
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

.rsoptions <- function(){
  fvobj <- mim.cmd("RSoptions", look.nice=FALSE, return.look.nice=TRUE)
  fvobj <- paste(fvobj,collapse='');
  value <- eval(parse(text=fvobj))
  return(value)
}

.rsmodel <- function(arg=NULL){
  fvobj <- mim.cmd(paste("RSmodel ",arg), look.nice=FALSE, return.look.nice=TRUE)
  fvobj <- paste(fvobj,collapse='');
  value <- eval(parse(text=fvobj))
  if (!is.na(value[1]))
    class(value)<-"modelInfo"    
  return(value)
}

.silent.as.numeric <-function(string.vec) {
    unlist(lapply(string.vec,function(x){x2<-type.convert(x); 
     if(is.factor(x2)||is.logical(x2)||is.complex(x2)) NA else x2}))
}

.split.mim.input <- function(input,token=NULL){
  s<-strsplit(input,'')[[1]]
  res <- NULL
  len <- 80
  while(length(s)>len){
    s1 <- s[1:len]
    res <- c(res, paste(paste(s1,collapse=''),token))
    s  <- s[-(1:len)]
  }
  value <- c(res, paste(s,collapse=''))
  return(value)
}




editmim <- function(x, add=NULL, hadd=NULL, del=NULL, letter=FALSE){
  .to <- function(str){
    str2 <-lapply(str, .partition.string.by,":")
    str3 <-.namesToLetters(str2,x$data)
    str3 <- unlist(unlist(lapply(str3, paste, collapse='')))
    return(str3)
  }
  if (letter==FALSE){
    add.let  <-    if (!is.null(add))  paste("Add",  paste(.to(add), collapse=","))
    hadd.let <-    if (!is.null(hadd)) paste("HAdd", paste(.to(hadd),collapse=","))
    del.let  <-    if (!is.null(del))  paste("Del",  paste(.to(del), collapse=","))
  } else {
    add.let  <-    if (!is.null(add))  paste("Add",  paste(add, collapse=","))
    hadd.let <-    if (!is.null(hadd)) paste("HAdd", paste(hadd,collapse=","))
    del.let  <-    if (!is.null(del))  paste("Del",  paste(del, collapse=","))
  }
  str  <- paste(add.let,";", hadd.let,";", del.let)
  mim.cmd(paste("Model ", .Formula.as.string(x)))
  mim.cmd(str, look.nice=FALSE)  
  value <- retrieveMIMvalues(x$data)
  return(value)
}


stepwise <- function(x,arg=NULL) UseMethod("stepwise")
stepwise.mim <- function(x,arg=NULL){
  .stepwiseMIM <- function(options=NULL,short=FALSE){
    mim.out <- mim.cmd(paste("stepwise ", options, collapse=''), look.nice=!short)
    index   <- min(grep("Selected", mim.out))
    if (short){
      value.str <- paste( mim.out[-(1:(index-1))], collapse=' ')
      cat(value.str,fill=TRUE)
    }
    value     <- mim.out[-(1:(index+1))]
    return(invisible(value))
  }
  
  fit(x)
  .stepwiseMIM(arg)
  value <- retrieveMIMvalues(x$data)
}

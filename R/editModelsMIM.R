.fixDefault <- function(edges=NULL,type,mim=NULL){
  str <- switch(type,
      "edges"={"FixEdges"},
      "set" ={"fix"})
 if (is.null(edges)){
  mim.cmd(paste("# ", str, edges),look.nice=FALSE)
  mim.cmd(paste(str, a),look.nice=FALSE)
 } else {
  s2<-unlist(strsplit(edges,",|\\+"))
  a<-lapply(s2, .namesToLetters,.getgmData(mim))
  a<-lapply(a,paste,collapse="")
  a<-paste(unlist(a),collapse=",")
  mim.cmd(paste("# ", str, edges),look.nice=FALSE)
  mim.cmd(paste(str, a),look.nice=FALSE)
 }
}

fixSet <- function(v=NULL,mim=NULL){
 .fixDefault(v,"set",mim)
}
fixEdges <- function(v=NULL,mim=NULL){
 .fixDefault(v,"edges",mim)
}


editmim <- function(x, deleteEdge=NULL, addEdge=NULL, haddEdge=NULL,
                    deleteTerm=NULL, addTerm=NULL, letter=FALSE){
  .to <- function(str){
    str2 <-lapply(str, .partition.string.by,":")
    str3 <-.namesToLetters(str2,x$data)
    str3 <- unlist(unlist(lapply(str3, paste, collapse='')))
    return(str3)
  }
  if (letter==FALSE){
    DE.let  <-    if (!is.null(deleteEdge))  paste("DeleteEdge",  paste(.to(deleteEdge), collapse=","))
    AE.let  <-    if (!is.null(addEdge))     paste("AddEdge",     paste(.to(addEdge),    collapse=","))
    HAE.let <-    if (!is.null(haddEdge))    paste("HAddEdge",    paste(.to(haddEdge),   collapse=","))
    DT.let  <-    if (!is.null(deleteTerm))  paste("DeleteTerm",  paste(.to(deleteTerm), collapse=","))
    AT.let  <-    if (!is.null(addTerm))     paste("AddTerm",     paste(.to(addTerm),    collapse=","))
  } else {
    DE.let  <-    if (!is.null(deleteEdge))  paste("DeleteEdge",  paste(deleteEdge, collapse=","))
    AE.let  <-    if (!is.null(addEdge))     paste("AddEdge",     paste(addEdge,    collapse=","))
    HAE.let <-    if (!is.null(haddEdge))    paste("HAddEdge",    paste(haddEdge,   collapse=","))
    DT.let  <-    if (!is.null(deleteTerm))  paste("DeleteTerm",  paste(deleteTerm, collapse=","))
    AT.let  <-    if (!is.null(addTerm))     paste("AddTerm",     paste(addTerm,    collapse=","))
  }
  str  <- paste(DE.let,";", AE.let,";", HAE.let,";", DT.let, ";",AT.let)
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

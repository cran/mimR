### BEGIN(EXPORT)
editmim <- function(x, deleteEdge=NULL, addEdge=NULL, haddEdge=NULL,
                    deleteTerm=NULL, addTerm=NULL){
  .to <- function(str){
    str2 <-lapply(str, .partition.string.by,":")
    str3 <-names2letters(str2,x$data)
    str3 <- unlist(unlist(lapply(str3, paste, collapse='')))
    return(str3)
  }
  DE.let  <-    if (!is.null(deleteEdge))
    paste("DeleteEdge",  paste(.to(deleteEdge), collapse=","))
  AE.let  <-    if (!is.null(addEdge))
    paste("AddEdge",     paste(.to(addEdge),    collapse=","))
  HAE.let <-    if (!is.null(haddEdge))
    paste("HAddEdge",    paste(.to(haddEdge),   collapse=","))
  DT.let  <-    if (!is.null(deleteTerm))
    paste("DeleteTerm",  paste(.to(deleteTerm), collapse=","))
  AT.let  <-    if (!is.null(addTerm))
    paste("AddTerm",     paste(.to(addTerm),    collapse=","))

  str  <- paste(DE.let,";", AE.let,";", HAE.let,";", DT.let, ";",AT.let)
  mim.cmd(paste("Model ", .Formula.as.string(x)))
  mim.cmd(str, look.nice=FALSE)  

  rsm <- .RSmodel()
  mimFormula <- rsm$mimFormula.as.string
  newmodel <- mim(mimFormula, data=x$data, fit=x$fit) 
  return(newmodel)
}

### END(EXPORT)

.partition.string.by <- function(string, token=NULL){
  ##print(".partition.string.by")
  if (is.null(string) || is.na(string))
    return(string)
  else{
    v<-unlist(strsplit(string,token))
    v<- v[unlist(lapply(v,nchar))!=0]
    return(v)
  }
}
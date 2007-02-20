.infConstant <- function(k=NULL){
  v <- mim.cmd(paste("InfConstant", k),look.nice=FALSE)
  v <- as.numeric(v[length(v)])
  return(v)
}


stepwise <- function(x,arg=NULL,critlevel=NULL,infconstant=NULL) UseMethod("stepwise")
stepwise.mim <- function(x,arg=NULL,critlevel=NULL,infconstant=NULL){
  
  .stepwiseMIM <- function(options=NULL,short=FALSE){
    ##mim.out <- mim.cmd(paste("stepwise ", options, collapse=''), look.nice=!short)
    mim.cmd(paste("# stepwise ", options, collapse=''), return.look.nice=TRUE)
    mim.out <- mim.cmd(paste("stepwise ", options, collapse=''), return.look.nice=TRUE)
    ##mo<<-print (mim.out)
#     index   <- min(grep("Selected", mim.out))
#     print(index)
#     if (short){
#       value.str <- paste( mim.out[-(1:(index-1))], collapse=' ')
#       cat(value.str,fill=TRUE)
#     }
#     value     <- mim.out[-(1:(index+1))]
#    return(invisible(value))
  }

  fit(x)

  if (!is.null(critlevel)){
    str <- paste("CritLevel ", critlevel)
    mim.cmd (print(str))
  }

  oldic <- .infConstant()
    if (!is.null(infconstant))    .infConstant(infconstant)
  .stepwiseMIM(arg)
  if (!is.null(infconstant))    .infConstant(oldic)


  rsm <- .RSmodel()
  mimFormula.letter <- rsm$Formula.as.string
  d <- x$data
  l <- string2listLetters(mimFormula.letter)
  a <- src2tgt(l, src=d$letter, tgt=d$name)
  mimFormulaNames <- list2stringNames(a)

  return(mim(mimFormulaNames, data=x$data))
}


fixSet <- function(v=NULL,mim=NULL){
 .fixDefault(v,"set",mim)
}
fixEdges <- function(v=NULL,mim=NULL){
 .fixDefault(v,"edges",mim)
}

.fixDefault <- function(edges=NULL,type,mim=NULL){
  str <- switch(type,
      "edges"={"FixEdges"},
      "set" ={"fix"})
 if (is.null(edges)){
  mim.cmd(paste("# ", str, edges),look.nice=FALSE)
  mim.cmd(paste(str, a),look.nice=FALSE)
 } else {
  s2<-unlist(strsplit(edges,",|\\+"))
  a<-lapply(s2, names2letters,.getgmData(mim))
  a<-lapply(a,paste,collapse="")
  a<-paste(unlist(a),collapse=",")
  mim.cmd(paste("# ", str, edges),look.nice=FALSE)
  mim.cmd(paste(str, a),look.nice=FALSE)
 }
}


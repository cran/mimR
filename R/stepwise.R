

stepwise.mim <- function(object, arg=NULL, critlevel=NULL, infconstant=NULL,...){

  x <- object
  .stepwiseMIM <- function(options=NULL,short=FALSE){
    ##mim.out <- mim.cmd(paste("stepwise ", options, collapse=''), look.nice=!short)
    mim.cmd(paste("# stepwise ", options, collapse=''), return.look.nice=TRUE)
    mim.out <- mim.cmd(paste("stepwise ", options, collapse=''),
                       look.nice=FALSE, return.look.nice=TRUE)
  }

  fit(x)

  if (!is.null(critlevel)){
    str <- paste("CritLevel ", critlevel)
    mim.cmd (str)
  }

  oldic <- .infConstant()
  if (!is.null(infconstant))
    .infConstant(infconstant)

  .stepwiseMIM(arg)

  if (!is.null(infconstant))
    .infConstant(oldic)

  rsm  <- .RSmodel()
  mimFormula.letter <- rsm$Formula.as.string
  d    <- x$data
  l    <- string2listLetters(mimFormula.letter)
  a    <- src2tgt(l, src=shortNames(d), tgt=varNames(d))
  mimFormulaNames <- list2stringNames(a)


  return(mim(mimFormulaNames, data=x$data))
}

.infConstant <- function(k=NULL){
  v <- mim.cmd(paste("InfConstant", k),look.nice=FALSE)
  v <- as.numeric(v[length(v)])
  return(v)
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


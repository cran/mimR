imputeMissing <- function(){
  mim.cmd("impute")
}

retrieveData <- function(arg="c"){
  value<- .RSprint(arg)
  names(value$Data) <- value$Variables$name[match(names(value$Data),value$Variables$letter)]
  value <- value$Data
  return(value)
}


variableType <- function(mim){
  is.discrete <- function(mim){
    used <- .used.names(mim)
    d <- mim$data
    a <- match(used,d$name)
    v <-all(d$factor[a])
    return(v)
  }
  
  is.continuous <- function(mim){
    used <- .used.names(mim)
    d <- mim$data
    a <- match(used,d$name)
    v <-all(!d$factor[a])
    return(v)
  }

  if (is.discrete(mim))
    value <- "discrete"
  else
    if (is.continuous(mim))
      value <- "continuous"
    else
      value <- "mixed"
  return(value)
}


.used.names <- function(mim,letter=FALSE){
  value <- unique(unlist(mim$modelInfo$Formula.as.list))
  if (letter==FALSE)
    value <- .lettersToNames(value, mim$data)
  return(value)
}

.latent.in.model <- function(mim){
  s <- mim$data
  used.names <- .lettersToNames(unique(unlist(mim$modelInfo$Formula.as.list)), s)
  v <- intersect(used.names, latent(s))
  value<-if (length(v)>0) v
  return(value)
}



###
### Miscellaneous stuff for mimR ver 1.01 (with gR-inspiration)
###

.q.by <- function(fvobj){
  q       <- fvobj$quadratic
  if (!is.null(q)){
    qt      <- as.data.frame.table(q)
    d.names <- names(dimnames(q)[-c(1,2)])
    c.names <- as.vector(dimnames(q)[[1]])
    q.by    <- by(qt, as.list(qt[,d.names,drop=FALSE]), function(a) a$Freq)
    return(q.by)
  }
}

.l.by <- function(fvobj){
  l <- fvobj$linear
  if (!is.null(l)){
    lt <- as.data.frame.table(l)
    d.names <- names(dimnames(l)[-c(1)])
    c.names <- as.vector(dimnames(l)[[1]])
    l.by       <- by(lt, as.list(lt[,d.names, drop=FALSE]), function(a) a$Freq)
    return(l.by)
  }
}

.d.by <- function(fvobj){
  d <- fvobj$discrete
  dt       <- as.data.frame.table(d)
  d.names  <- if (!is.vector(d)) names(dt)[1:(ncol(dt)-1)]
  d.by     <- by(dt, as.list(dt[,d.names,drop=FALSE]), function(a) a$Freq)
  return(d.by)
}

.d.names <- function(fvobj){
  d <- fvobj$discrete
  dt       <- as.data.frame.table(d)
  d.names  <- if (!is.vector(d)) names(dt)[1:(ncol(dt)-1)]
  return(d.names)
}

.d.levels <- function(fvobj){
  d <- fvobj$discrete
  d.levels <- if (!is.vector(d)) sapply(dimnames(d),length)
  return(d.levels)
}

.c.names <- function(fvobj){
  l <- fvobj$linear
  if (!is.null(l)){
    lt <- as.data.frame.table(l)
    c.names <- as.vector(dimnames(l)[[1]])
    return(c.names)
  }
}


############# NEW ###################


fitted.mim <- function(object, ...){
  fv      <- object$modelInfo$FittedValues
  is.homo <- object$modelInfo$Homogeneous
  if (variableType(object)=="continuous")
    v <- fv
  else
    v <- .generic.FittedValues(fv,is.homo)
  return(v)
}




returnValues <- function(object,type="fitted",marginal=NULL){
  log0 <- function(x){
    sapply(x,function(a)ifelse(a==0,0,log(a)))
  }
  div <- function(a,b){
    v <- a/b
    v[b==0]<-0
    return(v)
  }
  
  value <-
    switch(type,
           "fitted"=,"observed"=,"obs"={
             value <- .returnValuesPrimitive(object,type,marginal)
           },
           "deviance"=,"pearson"={
             if (variableType(object)=="discrete"){
               valo <- .returnValuesPrimitive(object,"observed",marginal)
               vale <- .returnValuesPrimitive(object,"fitted",marginal)
               ##print(cbind(valo,vale))
               o <- valo$Freq
               e <- vale$Freq
               ##print(o); print(e)
               switch(type,
                      "deviance"={
                        d <- -2 * o * log0( div(e,o) )},
                      "pearson"={
                        d <- div((o-e)^2,e) }
                      )
               valo$Freq <- d
               valo
             }else{
               stop("Deviance only implemented for discrete models")
             }
           }
           )
  return(value)
}

.returnValuesPrimitive <- function(object, type, marginal=NULL){
  value <- switch(type,
                  "fitted"={
                    .generic.FittedValues(object$modelInfo$FittedValues,
                            object$modelInfo$Homogeneous)},
                  "obs"=,"observed"=,"counts"={
                    .generic.FittedValues(object$suffStats)},
                  )
  
  if (!is.null(marginal)){
    if (variableType(object)=="discrete"){
      marginal <- gsub(' +','', marginal)
      marginal <- unlist(strsplit( marginal, '[ +: +]'))
      str   <- formula(paste("Freq ~ ",paste(marginal,collapse='+')))
      value <- as.data.frame(xtabs(str, data=value))
    } else {
      stop("Marginalization only implemented for discrete models")
    }
  }
  return(value)
}




.generic.FittedValues <- function(fv, is.homo=NULL){
  if (is.null(is.homo))
    is.homo <- FALSE

  dd <- .d.by(fv)
  ll <- .l.by(fv)
  qq <- .q.by(fv)

  counts <- as.data.frame(as.vector(dd))
  names(counts) <- "Freq"
  
  if (length(dd)>1){
    tab <- create.table(.d.levels(fv), .d.names(fv))
    value <- cbind(tab, counts)
  } else {
    value <- counts
  }
  if (!is.null(ll)){
    means<-as.data.frame(matrix(unlist(ll),
                                ncol=length(.c.names(fv)), byrow=TRUE))
    names(means) <- .c.names(fv) 
    covm <- matrix(unlist(qq), ncol=length(.c.names(fv))^2, byrow=TRUE)
    
    if (is.homo==TRUE){
      v <- NULL
      for (i in 1:nrow(means))
        v <- rbind(v, covm)
      covm <- v
    }
    
    covariances <- as.data.frame(covm)
    names(covariances)<-
      unlist(lapply(.c.names(fv),
                    function(x)paste(x,":", .c.names(fv),sep='')))
    value <- cbind(value, means, covariances)    
  }
  return(value)
}


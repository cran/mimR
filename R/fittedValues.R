### BEGIN(EXPORT)
fitted.mim <- function(object, ...){
  fv      <- object$modelInfo$FittedValues
  is.homo <- object$modelInfo$Homogeneous
  if (variableType(object)=="continuous")
    v <- fv
  else
    v <- .generic.FittedValues(fv,is.homo)
  return(v)
}

### END(EXPORT)
######## .functions below here ########################################


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


create.table <- function(levels,names=NULL){
  f <- function(levels){
    if (length(levels)==1)
      value <- return(1:levels)
    else{
      x <- levels[length(levels)]
      rest <- levels[1:(length(levels)-1)]
      rec.res <- f(rest)
      r2 <- NULL
      for (i in 1:x)
        r2 <- rbind(r2, cbind(rec.res,i))
      value <- r2
      
    }
    return(value)
  }
  value <- as.data.frame(f(levels))
  if (!is.null(names))
    names(value) <- names
  else
    names(value)    <-paste("x", 1:ncol(value),sep='')
  for (j in 1:ncol(value))
    value[,j] <- as.factor(value[,j])
  return(value)
}

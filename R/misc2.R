.used.names <- function(mim,letter=FALSE){
  value <- unique(unlist(mim$modelInfo$Formula.as.list))
  if (letter==FALSE)
    value <- .look.up.mim.names(value, mim$data, "from.mim")
  return(value)
}

.latent.in.model <- function(mim){
  s <- mim$data
  used.names <- .look.up.mim.names(unique(unlist(mim$modelInfo$Formula.as.list)), s,"from.mim")
  v <- intersect(used.names, latent(s))
  value<-if (length(v)>0) v
  return(value)
}


.partition.mim.input <- function(input,token=NULL){    
  curr     <- input
  n.char   <- 50 
  res <- NULL
    while(sum(nchar(curr))>n.char){
    cs <- cumsum(nchar(curr)+1)
    res <- c(res, paste(curr[cs<=n.char], collapse=' '))
    curr <- curr[!(cs<=n.char)]
  }
  value <- c(res, paste(curr, collapse=' '))
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


fitted.mim <- function(object, ...){
  
  is.homo <- object$modelInfo$Homogeneous
  fv <- object$modelInfo$FittedValues
  dd <- .d.by(fv)
  ll <- .l.by(fv)
  qq <- .q.by(fv)


  counts <- as.data.frame(as.vector(dd))
  names(counts) <- ".Counts"


  if (length(dd)>1){
    tab <- create.table(.d.levels(fv), .d.names(fv))
    value <- cbind(tab, counts)
  } else {
    value <- counts
  }
  if (!is.null(ll)){
    means<-as.data.frame(matrix(unlist(ll), ncol=length(.c.names(fv)), byrow=TRUE))
    names(means) <- .c.names(fv)

    covm <- matrix(unlist(qq), ncol=length(.c.names(fv))^2, byrow=TRUE)

    if (is.homo==TRUE){
      v <- NULL
      for (i in 1:nrow(means))
        v <- rbind(v, covm)
      covm <- v
    }

    covariances <- as.data.frame(covm)
    names(covariances)<-unlist(lapply(.c.names(fv), function(x)paste(x,":", .c.names(fv),sep='')))
    value <- cbind(value, means, covariances)    
  }
  return(value)
}


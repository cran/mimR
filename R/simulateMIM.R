
simulate     <- function(mim,size=1,digits=3) UseMethod("simulate")
simulate.mim <- function(mim,size=1,digits=3){
  fv <- mim$modelInfo$FittedValues
  is.homogeneous <- fv$homogeneous
  dd <- .d.by(fv)
  ll <- .l.by(fv)
  qq <- .q.by(fv)
  print(dd)
  p     <- as.numeric(dd)
  print(p)
  d.sim <- rmultinom(1, size=size, prob=p)
  tab   <- create.table(.d.levels(fv), .d.names(fv))
  
  res <- NULL
  for (i in 1:length(d.sim)){
    v <- matrix(rep(as.numeric(tab[i,]),d.sim[i]),ncol=ncol(tab),byrow=TRUE)
    res <- rbind(res,v)
  }
  res <- as.data.frame(res)
  names(res)<-.d.names(fv)
  for (j in 1:ncol(res)) res[,j] <- as.factor(res[,j])
  
  res2 <- NULL
  for (i in 1:length(d.sim)){
    if (d.sim[i]>0){
      if (is.homogeneous)
        S <- matrix(unlist(qq),ncol=ncol(tab))
      else
        S <- matrix(unlist(qq[[i]]),ncol=ncol(tab))
      m <- ll[[i]]
      print(d.sim[i])
      print(m)
      print(S)
      v<-round(mvrnorm(d.sim[i],m,S),digits)
      res2 <- rbind(res2,v)
    }
  }
  rownames(res2) <- NULL
  res2 <- as.data.frame(res2)
  names(res2)<-.c.names(fv)

  vl <- vallabels(m1$data)
  
  newrats<- cbind(res,res2)
  return(newrats)
}

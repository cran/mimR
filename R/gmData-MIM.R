
as.gmData.suffStats <- function(data, letter=NULL){

  nt <- data$Variables
  q.by <- .q.by(data)
  l.by <- .l.by(data)
  d.by <- .d.by(data)

  d.names <- .d.names(data)
  d.levels<- .d.levels(data)
  c.names <- .c.names(data)

  res <- NULL
  if ( length(d.by) > 1 ) {
    for (i in 1:length(d.by)){
      if (data$homogeneous==TRUE){
        curr.q <- q.by
      } else {
        curr.q <- q.by[[i]]
      }
      covm <- matrix(unlist(curr.q),ncol=length(l.by[[i]]));
      covm.tri <- t(covm)[!lower.tri(covm)];
      res <- rbind(res, c(d.by[[i]], l.by[[i]], covm.tri))
    }
  } else  {
    covm <- matrix(q.by[[1]],ncol=length(c.names))
    covm.tri <- t(covm)[!lower.tri(covm)]
    res <- rbind(res, c(as.numeric(d.by), unlist(l.by), unlist(covm.tri)))
  }

  attr(nt,"dataOrigin")     <- "suffStats"
  attr(nt,"observations")   <- res
  class(nt) <- c("gmData","data.frame")
  return(nt)
}


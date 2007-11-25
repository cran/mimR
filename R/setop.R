##
## Some set operations
##

cardOrder <- function(xlist){
  x <- xlist
  len <- unlist(lapply(x,length))
  unlen <- sort(unique(len))
  x2 <- NULL
  for (i in seq(along=unlen)){
    x2  <- c(x2, x[len==unlen[i]])
  }
  x2
}

is.subset <- function(x,y){
  setequal(intersect(x,y),x)
}

is.subsetList <- function(x,ylist){
  any(sapply(ylist, function(y) is.subset(x,y)))
}

maximalSet <- function(setlist){
  b     <- setlist
  b     <- rev(cardOrder(b))
  bnew  <- list()

  bnew.i  <- 1
  bnew    <- as.list(b[1])
  b[1]    <- NULL

  if (length(b)>0){
    for (i in 1:length(b)){
      ##cat("b[i]", paste(b[i]))
      ##print(bnew)
      ##print(is.subsetList(b[[i]], bnew))
      if (!is.subsetList(b[[i]], bnew)){
        bnew <- c(bnew, b[i])
      }
    }
  }
  bnew <- bnew[!is.na(bnew)]
  bnew <- bnew[!sapply(bnew, is.null)]

  return(bnew)
}




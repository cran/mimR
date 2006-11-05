##################################################################################
makeNameTable <- function(data) UseMethod("makeNameTable")
##################################################################################

makeNameTable.data.frame <- function(data){
  name      <- names(data)
  vallabels <- sapply(1:ncol(data), function(j) levels(data[,j]))
  names(vallabels) <- name
  factor    <- unlist(lapply(vallabels,length))

  vallabels <- vallabels[!unlist(lapply(vallabels, is.null))]
  
  value     <- makeNameTablePrimitive(name, factor=factor, vallabels=vallabels)
  return(value)
}

makeNameTable.table <- function(data){
  vallabels <- dimnames(data)
  name      <- names(vallabels)
  factor    <- unlist(lapply(vallabels,length))
  
  value <- makeNameTablePrimitive(name, factor=factor, vallabels=vallabels)
  return(value)
}

makeNameTablePrimitive <- function(name, letter=NULL,
                   factor=rep(FALSE,length(name)),
                   vallabels=NULL, data=NULL){
  if (is.null(letter))
    letter <- c(letters,LETTERS)[1:length(name)]

  levels            <- factor
  levels[levels==0] <- NA
  factor            <- factor!=0
  
  vallabels2 <-
    mapply(function(n,l)paste(n, 1:l,sep='.'), name[factor], levels[factor], SIMPLIFY=FALSE)

  if (length(vallabels2)>0){
    if (length(vallabels)>0 ){
      for (i in 1:length(names(vallabels))){
        ii<-match(names(vallabels)[i], names(vallabels2))
        if (length(ii)>0 && !is.na(ii))
          vallabels2[ii] <- vallabels[ii] 
      }
    }
  }
  
  value           <- data.frame(name,letter,factor,levels)
  value$name      <- as.character(name)
  value$letter    <- as.character(letter)
  rownames(value) <- 1:nrow(value)  
  attr(value,"vallabels")      <- vallabels2
  return(value)
}

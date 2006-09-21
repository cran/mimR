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


.silent.as.numeric <-function(string.vec) {
    unlist(lapply(string.vec,function(x){x2<-type.convert(x); 
     if(is.factor(x2)||is.logical(x2)||is.complex(x2)) NA else x2}))
}


.partition.string.by <- function(string, token=NULL){
  ##print(".partition.string.by")
  if (is.null(string) || is.na(string))
    return(string)
  else{
    ##print(string)
    ##print(class(string))
    ##print(token)
    v<-unlist(strsplit(string,token))
    v<- v[unlist(lapply(v,nchar))!=0]
    return(v)
  }
}
    
# .partition.string.by <- function(string, token=NULL){
#   if (is.null(string) || is.na(string))
#     return(string)
#   else{
#     string <- as.vector(string)
#     string <- gsub(' ', '', string)  ## spaces only
#     value <- NULL;
#     if (is.null(token)){
#       string<- paste(string,collapse='')
#       value <- sapply(1:nchar(string), function(i) substr(string,i,i))
#       return(value)
#     }
#     else{
#       i <- regexpr(token, string)
#       #if (i==-1)
#       if (i[1]==-1) 
#         return(string)
#       else{
#         while( i != -1){
#           sub.str <- substring(string,1,i-1) 
#           if (sub.str=="") sub.str <- NA
#           string  <- substring(string,i+1)
#           i <- regexpr(token, string)    
#           value <- c(value, sub.str)
#         }
#         if (nchar(string)>0)
#           value <- c(value, string)
#         return(value)
#       }
#     }
#   }
# }


.float.to.string <-
  function(num.vec,n.digits=6,width=9, preserve.int=TRUE){
    if (is.na(num.vec) || is.null(num.vec))
      return("*")
    else{
      if ((num.vec-round(num.vec))==0)
        return( sprintf("%g",num.vec) )
      else
        return( sprintf("%.5f",num.vec) ) 
    }
  }

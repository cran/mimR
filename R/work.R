modelInfo <- function(object){
  object$modelInfo
}
  
print.modelInfo <- function(x,...){
  nam<- names(x)
  len <- lapply(x,length)

  for (i in 1:length(nam)){
    if (len[i]==1){
      if (is.list(x[[i]])){
        cat("slot:", nam[i],"\n")
        print(x[[i]])
      } else
      cat("slot:", nam[i], ":", x[[i]],"\n")
    } else {
      cat("slot:", nam[i],"\n")
      print(x[[i]])
    }
  }
}






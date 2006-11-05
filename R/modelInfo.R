modelInfo <- function(object,slot=NULL){
  if (!is.null(slot))  
    value <- object$modelInfo[[slot]]
  else
    value <- object$modelInfo
  return(value)
}

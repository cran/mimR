
variableType <- function(object){
  is.discrete <- function(object){
    used <- .namesInModel(object)
    d <- object$data
    a <- match(used,d$name)
    v <-all(d$factor[a])
    return(v)
  }
  
  is.continuous <- function(object){
    used <- .namesInModel(object)
    d <- object$data
    a <- match(used,d$name)
    v <-all(!d$factor[a])
    return(v)
  }

  if (is.discrete(object))
    value <- "discrete"
  else
    if (is.continuous(object))
      value <- "continuous"
    else
      value <- "mixed"
  return(value)
}


######## .functions below here ########################################

.latentInModel <- function(object){
  s <- object$data
  used.names <- letters2names(unique(unlist(modelInfo(object,'Formula.as.list'))), s)
  v <- intersect(used.names, latent(s))
  value<-if (length(v)>0) v
  return(value)
}

.namesInModel <- function(object){
  value <- unique(unlist(modelInfo(object,'Formula.as.list')))
  value <- letters2names(value, object$data)
  return(value)
}



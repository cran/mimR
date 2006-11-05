
variableType <- function(mim){
  is.discrete <- function(mim){
    used <- .namesInModel(mim)
    d <- mim$data
    a <- match(used,d$name)
    v <-all(d$factor[a])
    return(v)
  }
  
  is.continuous <- function(mim){
    used <- .namesInModel(mim)
    d <- mim$data
    a <- match(used,d$name)
    v <-all(!d$factor[a])
    return(v)
  }

  if (is.discrete(mim))
    value <- "discrete"
  else
    if (is.continuous(mim))
      value <- "continuous"
    else
      value <- "mixed"
  return(value)
}


######## .functions below here ########################################

.latentInModel <- function(mim){
  s <- mim$data
  used.names <- letters2names(unique(unlist(modelInfo(mim,'Formula.as.list'))), s)
  v <- intersect(used.names, latent(s))
  value<-if (length(v)>0) v
  return(value)
}

.namesInModel <- function(mim){
  value <- unique(unlist(modelInfo(mim,'Formula.as.list')))
  value <- letters2names(value, mim$data)
  return(value)
}



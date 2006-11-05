###
### 2006-revision: Sufficient statistics to MIM
### 


momentstats <- function(factor=NULL, level=NULL, continuous=NULL, vallabels=NULL,
               counts=NULL, means=NULL, covariances=NULL,cmc=NULL){
  mcall <- match.call()
  mcall[[1]]<- as.name(".ssprimitive")
  val <- eval(mcall) 
  return(val)
}


.ssprimitive <- function(factor=NULL, level=NULL, continuous=NULL, vallabels=NULL,
               counts=NULL, means=NULL, covariances=NULL, cmc=NULL){

  val <- structure(list(factor=factor, level=level,
                        continuous=continuous, vallabels=vallabels,
                        counts=counts, means=means, covariances=covariances,
                        cmc=cmc),
                   class=c('momentstats','mixed'))

  if (is.null(factor) && is.null(continuous))
    stop("Either factors or continuous variables must be specified\n")
  
  if (is.null(continuous)){
    val$continuous <- val$means <- val$covariances <- NULL
    class(val) <- c('momentstats', 'discrete')
  }
  if (is.null(factor)){
    val$factor <- val$level <- val$vallabels  <- NULL
    class(val) <- c('momentstats', 'continuous')
  }

  if (!is.null(cmc)){
    val$counts <- val$means <- val$covariances <- NULL
  }
  
  return(val)
}

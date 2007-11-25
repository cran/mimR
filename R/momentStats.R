###
### 2006-revision: Sufficient statistics to MIM
### 


cellCounts <- function(varNames, nLevels=NULL, valueLabels=NULL, observations){
  structure(list(varNames=varNames,nLevels=nLevels, valueLabels=valueLabels, 
    observations=observations), class="cellCounts")
  
}

as.gmData.cellCounts <- function(from){
  g <- newgmData(from$varNames, nLevels=from$nLevels, 
                  observations=from$observations, valueLabels=from$valueLabels)
  attr(g,"dataOrigin") <- "cellCounts"
  return(g)
}

empCov <- function (S, counts=NULL, sd=NULL, mean=rep(0,ncol(S))){
  if (counts<=1)
    stop("There must be observations...")
  if (!is.null(sd)){
    S <- sd * t(sd * S)
  }
  structure(list(S=S, mean=mean, counts=counts), class="empCov")
}

as.gmData.empCov <- function(from){
  g <- newgmData(colnames(from$S), varTypes=rep("cont", length(colnames(from$S))), observations=from)
  attr(g,"dataOrigin") <- "empCov"
  return(g)
}










# momentstats <- function(factor=NULL, level=NULL, continuous=NULL, vallabels=NULL,
#                counts=NULL, means=NULL, covariances=NULL,cmc=NULL){
#   mcall <- match.call()
#   mcall[[1]]<- as.name(".ssprimitive")
#   val <- eval(mcall) 
#   return(val)
# }


# .ssprimitive <- function(factor=NULL, level=NULL, continuous=NULL, vallabels=NULL,
#                counts=NULL, means=NULL, covariances=NULL, cmc=NULL){

#   val <- structure(list(factor=factor, level=level,
#                         continuous=continuous, vallabels=vallabels,
#                         counts=counts, means=means, covariances=covariances,
#                         cmc=cmc),
#                    class=c('momentstats','mixed'))

#   if (is.null(factor) && is.null(continuous))
#     stop("Either factors or continuous variables must be specified\n")
  
#   if (is.null(continuous)){
#     val$continuous <- val$means <- val$covariances <- NULL
#     class(val) <- c('momentstats', 'discrete')
#   }
#   if (is.null(factor)){
#     val$factor <- val$level <- val$vallabels  <- NULL
#     class(val) <- c('momentstats', 'continuous')
#   }

#   if (!is.null(cmc)){
#     val$counts <- val$means <- val$covariances <- NULL
#   }
  
#   return(val)
# }



# as.gmData.momentstats <- function(data){
#   name    <- c(data$factor, data$continuous)
#   factid  <- rep(FALSE, length(name))
#   if (!is.null(data$factor))
#     factid[1:length(data$factor)] <- data$level

#   val <- gmData(name, factor=factid, vallabels=data$vallabels)  
#   attr(val,"dataOrigin")     <- "momentstats"
#   attr(val,"observations")   <- data
#   return(val)
# }


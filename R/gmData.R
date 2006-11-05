### BEGIN(EXPORT)
##################################################################################
as.gmData       <- function(data) UseMethod("as.gmData")
##################################################################################

gmData <- function(name, 
                   factor=rep(FALSE,length(name)),
                   vallabels=NULL){
  mcall        <- match.call()
  mcall[[1]]   <- as.name("makeNameTablePrimitive")
  value        <- eval(mcall,parent.frame())
  class(value) <- c("gmData","data.frame")  
  return(value)
}

as.gmData.data.frame <- function(data){
  nt <- makeNameTable(data)
  attr(nt,"dataOrigin"  )   <- "data.frame"
  attr(nt,"observations")   <- data
  class(nt)                 <- c("gmData","data.frame")
  return(nt)
  }

as.gmData.table <- function(data){
  nt <- makeNameTable(data)
  attr(nt,"dataOrigin")     <- "table"
  attr(nt,"observations")   <- data
  class(nt)                 <- c("gmData","data.frame")
  return(nt)
  }


as.gmData.momentstats <- function(data){
  name    <- c(data$factor, data$continuous)
  factid  <- rep(FALSE, length(name))
  if (!is.null(data$factor))
    factid[1:length(data$factor)] <- data$level

  val <- gmData(name, factor=factid, vallabels=data$vallabels)  
  attr(val,"dataOrigin")     <- "momentstats"
  attr(val,"observations")   <- data
  return(val)
}
### END(EXPORT)


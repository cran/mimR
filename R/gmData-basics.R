### BEGIN(EXPORT)
##################################################################################
as.gmData       <- function(data) UseMethod("as.gmData")
##################################################################################

gmData <- function(name, factor=rep(FALSE,length(name)),
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

### END(EXPORT)



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


retrieveData <- function(arg="c"){
  value<- .RSprint(arg)
  names(value$Data) <- value$Variables$name[match(names(value$Data),
                                                  value$Variables$letter)]
  value <- value$Data
  return(value)
}

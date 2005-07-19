.namesToLetters <- function(terms, names.table){
  #print(".namesToLetters")
  .look.up.mim.names(terms, names.table,direction="to.mim")
}

.lettersToNames <- function(terms, names.table){
  .look.up.mim.names(terms, names.table,direction="from.mim")
}

.look.up.mim.names <- function(terms, names.table,direction="from.mim"){
  if (length(terms)==0)
    return(NULL)
  else
    if (is.list(terms))
      return(lapply(terms,
                    .look.up.mim.names,
                    names.table=names.table, direction=direction))
    else {
      if (length(terms)>1){
        return(unlist(lapply(terms,
                             .look.up.mim.names,
                             names.table=names.table, direction=direction)))
      }
      else
        if (is.na(terms)) 
          return(NA)
        else{
          terms.list <- .partition.string.by(terms, ",")                              
          #print("terms"); print(terms)
          #print("terms.list"); print(terms.list)
          switch(direction,
                 from.mim = {
                   a <- lapply(terms.list, function(x1){
                     v <- .partition.string.by(x1)
                     v2<- paste(names.table$name[match(v, names.table$letter)])
                   }
                               )
                   
                 },
                 to.mim = {
                   a <- lapply(terms.list, function(x1){
                     v <- .partition.string.by(x1,":")
                     v2<- paste(names.table$letter[match(v, names.table$name)])
                     v2
                   }
                               )
                   
                 }
                 )
                                        #print(a)
          return(unlist(a))
        }
    }
}

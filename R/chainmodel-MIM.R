
chainmim <- function(mimFormula.list, data){    
  tmp.data <- data
  observations(tmp.data) <- NULL
  attr(tmp.data,"dataOrigin") <- "no.data"
  
  mim.list <- lapply(mimFormula.list,
                     function(mf){ mod <- mim(mf, tmp.data) })
  
  sets <- lapply(mim.list, .used.names)
  blocks <- sets
  for (j in 2:length(blocks))
    blocks[[j]] <- setdiff(blocks[[j]], blocks[[j-1]])
  
  s <- lapply(blocks, .namesToLetters, data)    
  s <- unlist(lapply(s, paste, collapse=' '))
  block.str <- paste(s, collapse='|')    
  
  formula.str.list <- lapply(mim.list, .Formula.as.string)
  formula.str.list <- paste(formula.str.list, collapse='|')

  print(block.str)
  print(formula.str.list)
  
  toMIM(data)
                                        
  mim.cmd(paste("setblock", block.str))
  mim.cmd("BlockMode +")
  mim.cmd(paste("BRmodel ", formula.str.list))
  value <- list("mim.list"=mim.list)
  return(mim.list)    
}

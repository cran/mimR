
".mim.setblock" <-
function(br.structure=NULL,data=NULL,mim.names=FALSE){
  if (is.null(br.structure)){
    value <- paste("SetBlock")
    mim.cmd(value)
  }
  else{
    if (!is.null(data)){
      if (length(br.structure)>1)
        br.structure <- paste(br.structure, collapse=' | ')
      ##print(br.structure)
      if (mim.names==FALSE){
        names.table <- data$names.table
        last.token <- ifelse (is.null(names.table), "", ",") 
        s1 <- lapply(.partition.string.by(br.structure, "\\|"),
                     .partition.string.by, last.token)
        s2 <- lapply(s1, function(a){
          b <- .namesToLetters(a,names.table=names.table, direction="to.mim")
          paste(as.vector(b),collapse='')}) 
        s3 <- paste(s2, collapse="|")
      }
      else{
        s3 <- br.structure
      }
      value <- paste("SetBlock", s3)        
      mim.cmd(value)
    }
    else
      value <- NULL
  }
  print(value)
  return(invisible(value))
}



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

.formula.toMIM <- function(object){
  ##mim.cmd(paste("# Model", object))
  ##mim.cmd(paste("Model", .Formula.as.string(object)))

  str  <- paste("Model ", .Formula.as.string(object))
  str2 <- .str2strlist(str)
  lapply(str2, mim.cmd)
}

.varspec.toMIM <- function(data,text=""){
  mim.cmd("clear; clear output")
  mim.cmd("# ### .varspec.toMIM")
  
  mim.cmd(text)
  vs  <- .findVarspec(data) ## .namesTable.to.varspec (data)
  lapply(vs, function(s){if(!is.null(s)) mim.cmd(s, look.nice=FALSE)})

#   v <- attr(data,"ordinal")
#   if (!is.null(v)){
#     s<-paste("# Ordinal", paste(v, collapse=" "));
#     mim.cmd(s, look.nice=FALSE)
#     v2 <- names2letters(v, data)
#     s<-paste("Ordinal", paste(v2, collapse=" "));
#     mim.cmd(s, look.nice=FALSE)
#   }

  mim.cmd("% .varspec.toMIM DONE");
}





.formula.toMIM <- function(object){
  mim.cmd(paste("# Model", object))
  mim.cmd(paste("Model", .Formula.as.string(object)))
}

.varspec.toMIM <- function(data,text=""){
  mim.cmd("clear; clear output")
  mim.cmd(text)
  vs  <- .namesTable.to.varspec (data)
  lapply(vs, function(s){if(!is.null(s)) mim.cmd(s)})

  v <- attr(data,"ordinal")
  if (!is.null(v)){
    s<-paste("# Ordinal", paste(v, collapse=" "));
    mim.cmd(s, look.nice=FALSE)
    v2 <- names2letters(v, data)
    s<-paste("Ordinal", paste(v2, collapse=" "));
    mim.cmd(s, look.nice=FALSE)
  }

  mim.cmd("% .varspec.toMIM DONE");
}





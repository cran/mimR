.formula.toMIM <- function(mim){
  mim.cmd(paste("# Model", mim))
  mim.cmd(paste("Model", .Formula.as.string(mim)))
}

.varspec.toMIM <- function(data,text=""){
  mim.cmd("clear; clear output")
  mim.cmd(text)
  vs  <- .namesTable.to.varspec (data)
  lapply(vs, function(s){if(!is.null(s)) mim.cmd(s)})
  mim.cmd("% .varspec.toMIM DONE");
}





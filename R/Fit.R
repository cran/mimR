fit <- function(mim){
  if (!is.null(.latent.in.model(mim)))
    stop("Model has latent variable and 'fit' can not be used - try using 'emfit'")
  toMIM(mim$data)
  .formula.toMIM(mim)
  v <- .fit()
  mim <- .retrieve.fittedMIM(mim)
  mim$deviance <- v[1]
  return(mim)
}

emfit <- function(mim,arg="R", emconv=0.0001, emmax=1000){
  toMIM(mim$data)
  .initLatent(latent(mim$data), mim$data)
  .formula.toMIM(mim)
  cat("Fitting using EM algorithm...\n")
  str <- paste("EMconv", sprintf("%.12f", emconv), "; EMmax", emmax)
  #print(str)
  mim.cmd(str)
  mim.cmd(paste("Emfit", arg))
  mim <- .retrieve.fittedMIM(mim)
  return(mim)
}


.retrieve.fittedMIM <- function(mim){
  v1<- .rsmodel()
  v2<- .rsprint.suffStats()
  v2$Variables <- v1$Variables
  mim$modelInfo     <- v1;
  mim$suffStats     <- v2;
  return(mim)
}

.formula.toMIM <- function(mim)
  mim.cmd(paste("Model", .Formula.as.string(mim)))
  

.fit <- function(){
  .mim.output.to.list <- function(mo){
    a1 <- .silent.as.numeric(mo)
    value  <- a1[which(!is.na(a1))]
    string <- mo[which(is.na(a1))]
    string <- as.vector(sapply(string, .partition.string.by, ":"))
    names(value) <- tolower(string)
    value <- as.list(value)
  }

  v <- mim.cmd("fit")
  if(length(grep("Insufficient",v)>0))
    stop("MIM is unable to fit the model",call.=FALSE)
  else
    .mim.output.to.list(v)
}

.initLatent <- function(set, data){
    s <- .look.up.mim.names(set, data, "to.mim")
    cat("Initializing latent variables:", paste(set, collapse=' '), "\n")
    s2<- paste("calc", s, "=", s, "+ln(0);")
    mim.cmd(s2)
}



fit <- function(mim,submitData=TRUE){
  if (!is.null(.latent.in.model(mim)))
    stop("Model has latent variable and 'fit' can not be used - try using 'emfit'")

  if (submitData==FALSE){
    #cat("WARNING: Data are not entered to MIM engine. This is not a problem\n")
    #cat(" if the relevant data are already loaded in MIM, but no checking is performed...\n")
  }
  else
    toMIM(mim$data)

  .formula.toMIM(mim)
  v <- .fit()
  mim <- .retrieve.fittedMIM(mim)
  mim$deviance <- v[1]
  return(mim)
}

emfit <- function(mim,arg="R", submitData=TRUE, emconv=0.0001, emmax=1000,plot=FALSE){
  if (submitData==FALSE){
    #cat("WARNING: Data are not entered to MIM engine. This is not a problem\n")
    #cat(" if the relevant data are already loaded in MIM, but no checking is performed...\n")
  }
  else
    toMIM(mim$data)
  if (toupper(arg)=="S")
    .initLatent(latent(mim$data), mim$data)
  .formula.toMIM(mim)
  cat("Fitting using EM algorithm... ")
  str <- paste("EMconv", sprintf("%.12f", emconv), "; EMmax", emmax)
  mim.cmd(str, look.nice=FALSE)
  res <- .mim.cmd.term(paste("Emfit", arg),look.nice=FALSE)

  result<-
    rbind(
          c(as.numeric(res[9:10]), NA),
          matrix(as.numeric(res[11:(which(res=="Successful")-1)]),ncol=3,byrow=TRUE)
          )
  result<-as.data.frame(result)
  names(result)<-c("cycle","m2logL","change")
  if (plot != FALSE){
    par(mfrow=c(1,2))
    plot(result$cycle,result$m2logL,xlab="Iteration",ylab="-2logL"); title("-2 log Likelihood");
    lines(result$cycle,result$m2logL)
    plot(result$cycle,result$change,xlab="Iteration",ylab="Change"); title("Change in log Likelihood")
    lines(result$cycle,result$change)
  }
  cat(nrow(result),"iterations\n")
  mim <- .retrieve.fittedMIM(mim)
  mim$EMconvergence <- result
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



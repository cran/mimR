testdelete <- function(edge,mim,options=NULL){
  d <- .dataMIM(mim)
  e <- .namesToLetters(edge,d)
  .RStestdelete(e,options)
}

fit <- function(mim,submitData=TRUE){
  if (!is.null(.latent.in.model(mim)))
    stop("Model has latent variable and 'fit' can not be used - try using 'emfit'")
  
  if (submitData==FALSE){
    ##cat("WARNING: Data are not entered to MIM engine. This is not a problem\n")
    ##cat(" if the relevant data are already loaded in MIM, but no checking is performed...\n")
  }
  else
    toMIM(mim$data)

  .formula.toMIM(mim)
  v <- .fit()
  mim <- .retrieve.fittedMIM(mim)
  mim$deviance <- v[1]
  return(mim)
}

.emfitMIM <- function(arg="R",plot=FALSE){
    res<-mim.cmd(paste("emfit ", arg, sep=' '), look.nice=TRUE)
    result<-
    rbind(
        c(as.numeric(res[9:10]), NA),
        matrix(as.numeric(res[11:(which(res=="Successful")-1)]),ncol=3,byrow=TRUE)
    )
    result<-as.data.frame(result)
    names(result)<-c("cycle","m2logL","change")
    if (plot != FALSE){
        par(mfrow=c(1,2))
        plot(result$cycle,result$m2logL); title("-2 log Likelihood");
        lines(result$cycle,result$m2logL)
        plot(result$cycle,result$change); title("Change in log Likelihood")
        lines(result$cycle,result$change)
    }
    value <- result
    return(invisible(value))
}

emfit <- function(mim,arg="R", submitData=TRUE, emconv=0.0001, emmax=1000,plot=FALSE,info=FALSE){
  time.start <- proc.time()
  if (submitData==FALSE){
    ##cat("WARNING: Data are not entered to MIM engine. This is not a problem\n")
    ##cat(" if the relevant data are already loaded in MIM, but no checking is performed...\n")
  }
  else
    toMIM(mim$data)

  if (info==TRUE)
    cat("Fitting using EM algorithm... ")
  
  .initLatent(latent(mim$data), mim$data, info)
  .formula.toMIM(mim)

  str <- paste("EMconv", sprintf("%.12f", emconv), "; EMmax", emmax)
  if (info==TRUE)
    cat(str,"\n")
  
  mim.cmd(str, look.nice=FALSE)
  
  res <- .mim.cmd.term(paste("Emfit", arg),look.nice=FALSE)
  rrr <- grep(c("initialisation."), res)
  if (toupper(arg)=="S" && length(rrr)>0){
    cat ("Initialization with given starting values failed - trying with random starting values\n")
    res <- .mim.cmd.term(paste("Emfit", "r"),look.nice=FALSE)
  }

  rrr    <- match(c("Successful","Error","exceeded."), res)
  theend <- match(c("Successful","Maximum"), res)
  endidx <- theend[which(!is.na(theend))]

  result <- rbind(
                  c(as.numeric(res[9:10]), NA),
                  matrix(as.numeric(res[11:(endidx-1)]),ncol=3,byrow=TRUE)
                  )  

  result<-as.data.frame(result)
  names(result)<-c("cycle","m2logL","change")

  if (plot != FALSE){
    opar <- par("mfrow")
    par(mfrow=c(1,2))
    plot(result$cycle,result$m2logL,xlab="Iteration",ylab="-2logL");
    title("-2 log Likelihood");
    lines(result$cycle,result$m2logL)
    plot(result$cycle,result$change,xlab="Iteration",ylab="Change");
    title("Change in log Likelihood")
    lines(result$cycle,result$change)
    par(mfrow=opar)
  }
  mim <- .retrieve.fittedMIM(mim)

  if (info==TRUE){
    if(res[endidx]=="Maximum")
      cat("Note: maximum number of iterations exceeded\n")
    cat("Iterations", nrow(result), "Time",   (proc.time()-time.start)[3], "\n")
  }
  mim$EMconvergence <- result 
  return(mim)
}


.retrieve.fittedMIM <- function(mim){
  v1<- .RSmodel()
  v2<- .RSprint.suffStats()
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

.initLatent <- function(set, data, info=FALSE){
    s <- .namesToLetters(set, data)
    if (info==TRUE)
      cat("Initializing latent variables:", paste(set, collapse=' '), "\n")
    s2<- paste("calc", s, "=", s, "+ln(0);")
    mim.cmd(s2)
}



#.fitMIM <- function (){
#  mim.output <- mim.cmd("fit", look.nice=FALSE)
#  res <- .silent.as.numeric( mim.output[c(2,4)] )
#  value <- c("deviance"=res[1], "df"=round(res[2],0))
#  return(print(value))
#}

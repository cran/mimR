linpredict       <- function(mim,y,x=NULL,letter=FALSE,
                             submitData=TRUE, submitModel=TRUE)
  UseMethod("linpredict")

linpredict.mim   <- function(mim,y,x=NULL,letter=FALSE,
                             submitData=TRUE, submitModel=TRUE)    {
  #cat("linpredict.mim\n")

  if (submitData==FALSE){
    #cat("WARNING: Data are not entered to MIM engine. This is not a problem\n")
    #cat(" if the relevant data are already loaded in MIM, no checking is performed...\n")
  } else {
    if (!is.null(mim$suffStats)){
      cat("Entering sufficient statistics... ")
      toMIM(as.gmData(mim$suffStats)) 
    } else {
      cat("Entering raw data... ")
      toMIM(mim$data)
    }
    cat("done\n")
  }

  nt <- mim$data
  if (submitModel==TRUE){
    mim.cmd(paste("Model ", mim$mimFormula.letter))
    mim.cmd("Fit")
  } else {
    #cat("WARNING: Model is not being fitted by the MIM engine. This is not a problem\n")
    #cat(" if the relevant model is alread fitted in MIM, but no checking is performed...\n")
  }

  if (letter==TRUE){
    x.letter <- x
    y.letter <- y
    x.name <- if(!is.null(x))
      .lettersToNames(x, nt)
    y.name <- .lettersToNames(y, nt)
  } else {
    x.letter <- if(!is.null(x))
      .namesToLetters(paste(x,collapse=':'), nt)
    y.letter <- .namesToLetters(paste(y,collapse=':'), nt)
    x.name  <- x
    y.name  <- y
  }

  mim.cmd("Printformat 15,7", look.nice=FALSE)
  value <- displayMIM(y.letter, x.letter)
  value$y <- y.name;
  value$x <- x.name
  value$names.table <- nt
  value <- .display.to.df.names(value)

  value$type.text <-
    paste("Distribution of", paste(y.name,collapse=' '),
          if (!is.null(x.name)) paste("given", paste(x.name,collapse=' ')))
  return(value)
}



.display.to.df.names <- function(dd){
  nt<- dd$names.table
  y <- .partition.string.by(dd$y,":");
  x <- .partition.string.by(dd$x,":");

  if (dd$variable.type=="mixed")
    for (i in 1:length(dd$stats)){
      dd1 <- dd$stats[[i]]
      new.col.names <- c("int", unlist(.lettersToNames(colnames(dd1$means)[-1],nt)))
      dimnames(dd1$means) <- list(y, new.col.names)
      dimnames(dd1$cov) <- list(y,y)
      dd1$disc.names <- unlist(.lettersToNames(dd1$disc.names,nt))
      dd$stats[[i]] <- dd1
    }
  else {
    if (dd$variable.type=="continuous"){
      new.col.names <- c("int", unlist(.lettersToNames(colnames(dd$means)[-1],nt)))
      dimnames(dd$means) <- list(y, new.col.names)
      dimnames(dd$cov) <- list(y,y)
      value <- dd
    } else {
      nn <- names(dd$table)
      C.i <- match("Constant", names(dd$table))
      if (!is.na(C.i)){
        nn[1:(C.i-1)] <- y
        nn[(C.i+1):length(nn)] <- x
      } else {
        nn[1:(length(nn)-1)] <- .lettersToNames(nn[-length(nn)],nt)
      }
      ##print(nn)
      names(dd$table) <- nn
    }
  }
  
  return(invisible(dd))
}



"print.linpredictMIM" <-
function(x,...){
  cat(x$type.text, fill=TRUE)
  switch(x$variable.type,
         discrete  ={
           print(x$table)},
         continuous={
           x.cov <- x$cov;
           x.mean<- x$means
           print(x.mean);
           print(x.cov);
         },
         mixed     ={
           lapply(x$stats,
                  function(xx){
                    cat(paste(paste(xx$disc.names,collapse=","), "=",
                              paste(xx$disc.levels,collapse=","), sep=''),fill=TRUE)
                    x.cov <- xx$cov;
                    x.mean<- xx$means
                    print(x.mean)
                    print(x.cov)
                  }
                  )
         }
         )
  return(invisible(x))
}

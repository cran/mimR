
testdelete <- function(edge,object,arg=NULL){

  options <- arg
  d     <- .getgmData(object)
  edge2 <- unlist(strsplit(edge,":"))
  e     <- names2letters(edge2,d)
  fit(object)
  val   <- .RStestdelete(e,options)

  ## print("testdelete -- val:::"); print(val)
  
  if (identical(val,NA))
    cat("Edge ",edge, " can not be deleted (maybe it is not in the model).\n")
  else {
    val$df <- abs (val$df1 - val$df2)
    val$df1 <- val$df2 <- NULL

    ## THIS IS A HACK
    options <- tolower(options)
    if (length(grep("k", options))>0 ||
        length(grep("w", options))>0 ||
        length(grep("j", options))>0){
      s <- paste("testdelete ", paste(e,collapse=""), options)
      v <- mim.cmd(s, look.nice=FALSE)
      ##print(v)
      v <- as.numeric(v[length(v)])
      ##cat ("P before:", val$P, "\n")
      val$P <- v
      ##cat ("P after:", val$P, "\n")
    }
    .testprint(val)
  }
  return(invisible(val))
}

modelTest <- function(m1,m2=NULL) UseMethod("modelTest")
modelTest.mim <- function(m1,m2=NULL){

  ms1 <- modelInfo(m1,"Formula.as.string")
  fit(m1)  
  if (!is.null(m2)){
    ms2 <- modelInfo(m2,"Formula.as.string")
    mim.cmd("base")
    mim.cmd(paste("model ",ms2))
    val <- .RStest()

    if (identical(val,NA)){
      cat("Can not compare models ",formula(m1), " and ", formula(m2),"...\n")
    } else {
      if (DF(m2) > DF(m1)){
        cat("Test of H0 : ", formula(m1),"\n")
        cat("Against    : ", formula(m2),"\n\n")
      } else {
        cat("Test of H0 : ", formula(m2),"\n")
        cat("Against    : ", formula(m1),"\n\n")
      }
      val$df  <- abs (val$df1 - val$df2)
      val$df1 <- val$df2 <- NULL
      .testprint(val)    
    }
  } else {
    if (identical(val,NA)){
      cat("Can not compare model ",formula(m1), " to the saturated model...\n")
    } else {
      cat("Test of H0 : ", formula(m1),"\n")
      cat("Against saturated model \n")
      val$df  <- abs (val$df1 - val$df2)
      val$df1 <- val$df2 <- NULL
      .testprint(val)    
    }
  }
  return(invisible(val))
}


.testprint <- function(v1){
  v1 <- v1[c("test","method", "stat","df","P")]
  cat(paste(names(v1[1:2]),v1[1:2], sep=": "),"\n")
  cat(paste(names(v1[-(1:2)]),v1[-(1:2)], sep=": "),"\n")
  
}

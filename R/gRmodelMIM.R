
.is.fitted            <- function(x) ifelse (is.null(x$modelInfo), FALSE, x$modelInfo$Fitted)
.mimFormula           <- function(x) x$mimFormula
.mimFormula.letter    <- function(x) x$mimFormula.letter
.Formula.as.string    <- function(x) x$modelInfo$Formula.as.string
.DF                   <- function(x) x$modelInfo$DF
.likelihood           <- function(x) x$modelInfo$likelihood
.cliques              <- function(x) x$modelInfo$Cliques

DF.mim                <- function(x) x$modelInfo$DF
DF                    <- function(x) UseMethod("DF")

likelihood            <- function(x) x$modelInfo$likelihood
cliques               <- function(x) x$modelInfo$Cliques
deviance.mim          <- function(object, ...) object$deviance


.parse.mimFormula <- function(mimFormula, letter=FALSE){
  # Turns a formula (as a string) into a list with discrete, linear and quadratic generators
  s1 <- unlist(strsplit(mimFormula, "/"))
  if (length(s1)==2)    s1 <- s1[1]
  s1 <- c(s1, NA, NA)[1:3]

  if (letter==FALSE){
    s3 <-lapply(s1, function(s){
      if(is.na(s))
        return(s)
      else {
        s2 <- strsplit(s, "\\+")    
        s2 <- unlist(lapply(s2, function(x)gsub(" +","",x)))
        s3 <- strsplit(s2,":")
        return(s3)
      }
    })
    
  } else {
    s3 <-lapply(s1, function(s){
      if(is.na(s))
        return(s)
      else {
        s2 <- strsplit(s, "[+,]")    
        s2 <- unlist(lapply(s2, function(x)gsub(" +","",x)))
        s3 <- strsplit(s2,"")
        return(s3)
      }
    })
  }
  names(s3) <- c("discrete", "linear", "quadratic")
  return(s3)        
}


.parsedFormula.to.string <- function(mfp,nt,input="name",output="name"){
  #Turns a formula (as a list) into a string.
  if (output=="name"){
    if (input=="name")
      mfl <-mfp
    else
      mfl <- lapply(mfp, .look.up.mim.names, direction="from.mim",names.table=nt)
    aaa<- lapply(mfl, function(x) paste(unlist(lapply(x, paste, collapse=':')),collapse='+'))
  }
  else{
    if (input=="name")
      mfl <- lapply(mfp, .look.up.mim.names, direction="to.mim",names.table=nt)
    else
      mfl <- mfp
    aaa<- lapply(mfl, function(x) paste(unlist(lapply(x, paste, collapse='')),collapse=','))
  }
  aaa<- unlist(aaa)
  aaa[aaa=="NA"]<-""
  mim.str<-paste(aaa, collapse="/")
  return(mim.str)
}


mim <- function(mimFormula, data, letter=FALSE, marginal=data$name){

  mim.cmd("clear; clear output")
  vs  <- .nt.to.varspec (data)
  lapply(vs, function(s){if(!is.null(s)) mim.cmd(s)})
  
  if( !is.na(match(mimFormula, c("*", "*h", ".")))){
    marg <- if (letter==FALSE)
      .look.up.mim.names(marginal, data, "to.mim")
    else 
      marginal
    model.type <-switch(mimFormula, "*"={"SatMod"}, "*h"={"HomSatMod"}, "."={"Main"})
    str <- paste(model.type, paste(marg,collapse=' '))
    mim.cmd(str)
    rsm               <- .rsmodel();
    ##print(rsm)

    mimFormula.letter <- rsm$Formula.as.string
    value <-.make.mim(mimFormula.letter, data, letter=TRUE, rsm=rsm);
    
  } else {
    tmp <-.make.mim(mimFormula, data, letter=letter)
    rsm   <- .rsmodel(tmp$mimFormula.letter)
    value <-.make.mim(mimFormula, data, letter=letter,rsm=rsm)
  }
  return(value)
}


.make.mim <- function(mimFormula, data, letter=FALSE, rsm=NULL,rsp=NULL){
  ### DOES NOT CALL MIM
  x1<-.parse.mimFormula(mimFormula,letter=letter)
  if (letter==TRUE){
    mimFormula.letter <- mimFormula
    mimFormula.name <-.parsedFormula.to.string(x1,nt=data,input="letter", output="name")
  } else {
    mimFormula.name <- mimFormula
    mimFormula.letter  <- .parsedFormula.to.string(x1,nt=data,output="letter",
                                                   input=ifelse(letter==FALSE,"name","letter"))
  }
  value <-list("mimFormula"=mimFormula.name, "mimFormula.letter"=mimFormula.letter,
               "modelInfo"=rsm, "data"=data)
  if (!is.null(rsp))
    if (!is.na(rsp) && !is.na(rsm)){
      rsp$Variables <- rsm$Variables
      value$suffStats <- rsp
    }
  value$used.names <- .used.names(value)
  class(value) <- "mim"
  return(value) 
}

retrieve <- function(data=NULL){
  rsm <- .rsmodel()
  rsp <- .rsprint.suffStats()
  if (is.na(rsm[1])){
    return(NULL)
  } else { 
    if (is.null(data)){
      if (!is.na(rsp[1])){
        rsp$Variables <- rsm$Variables
        data <- as.gmData(rsp)
      } else {
        data <- .nt.as.gmData(rsm$Variables)
      }
    }    
    mimFormula.letter <- rsm$Formula.as.string
    x1 <- .parse.mimFormula(mimFormula.letter,letter=TRUE)
    mimFormula.name <-.parsedFormula.to.string(x1,data,input="letter", output="name")
    value <- .make.mim(mimFormula.letter, data, letter=TRUE, rsm=rsm,rsp=rsp)
    return(value)
  }
}



stepwise <- function(x,arg=NULL) UseMethod("stepwise", x, arg)

stepwise.mim <- function(x,arg=NULL){
  .stepwiseMIM <- function(options=NULL,short=FALSE){
    mim.out <- mim.cmd(paste("stepwise ", options, collapse=''), look.nice=!short)
    index   <- min(grep("Selected", mim.out))
    if (short){
      value.str <- paste( mim.out[-(1:(index-1))], collapse=' ')
      cat(value.str,fill=TRUE)
    }
    value     <- mim.out[-(1:(index+1))]
    return(invisible(value))
  }
  
  fit(x)
  .stepwiseMIM(arg)
  value <- retrieve(x$data)
}






editmim <- function(x, add=NULL, hadd=NULL, del=NULL, letter=FALSE){
  .to <- function(str){
    str2 <-lapply(str, .partition.string.by,":")
    str3 <-.look.up.mim.names(str2,x$data, "to.mim")
    str3 <- unlist(unlist(lapply(str3, paste, collapse='')))
    return(str3)
  }
  if (letter==FALSE){
    add.let  <-    if (!is.null(add))  paste("Add",  paste(.to(add), collapse=","))
    hadd.let <-    if (!is.null(hadd)) paste("HAdd", paste(.to(hadd),collapse=","))
    del.let  <-    if (!is.null(del))  paste("Del",  paste(.to(del), collapse=","))
  } else {
    add.let  <-    if (!is.null(add))  paste("Add",  paste(add, collapse=","))
    hadd.let <-    if (!is.null(hadd)) paste("HAdd", paste(hadd,collapse=","))
    del.let  <-    if (!is.null(del))  paste("Del",  paste(del, collapse=","))
  }
  str  <- paste(add.let,";", hadd.let,";", del.let)
  mim.cmd(paste("Model ", .Formula.as.string(x)))
  mim.cmd(str, look.nice=FALSE)  
  value <- retrieve(x$data)
  return(value)
}


simulate     <- function(mim,size,digits=3) UseMethod("simulate")

simulate.mim <- function(mim,size,digits=3){
  fv <- mim$modelInfo$FittedValues
  is.homogeneous <- fv$homogeneous
  dd <- .d.by(fv)
  ll <- .l.by(fv)
  qq <- .q.by(fv)
  
  p     <- as.numeric(dd)
  d.sim <- rmultinom(1, size=size, prob=p)
  tab   <- create.table(.d.levels(fv), .d.names(fv))
  
  res <- NULL
  for (i in 1:length(d.sim)){
    v <- matrix(rep(as.numeric(tab[i,]),d.sim[i]),ncol=ncol(tab),byrow=TRUE)
    res <- rbind(res,v)
  }
  res <- as.data.frame(res)
  names(res)<-.d.names(fv)
  for (j in 1:ncol(res)) res[,j] <- as.factor(res[,j])
  
  res2 <- NULL
  for (i in 1:length(d.sim)){
    if (d.sim[i]>0){
      if (is.homogeneous)
        S <- matrix(unlist(qq),ncol=ncol(tab))
      else
        S <- matrix(unlist(qq[[i]]),ncol=ncol(tab))
      m <- ll[[i]]
      v<-round(mvrnorm(d.sim[i],m,S),digits)
      res2 <- rbind(res2,v)
    }
  }
  rownames(res2) <- NULL
  res2 <- as.data.frame(res2)
  names(res2)<-.c.names(fv)
  
  newrats<- cbind(res,res2)
  return(newrats)
}

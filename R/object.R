
###
### mimModel

mim.model <- function(mim.formula,data, mim.names=TRUE, submit=TRUE,
                      submit.data=TRUE) {

  make.mim.model.object <- function(mim.formula, data,mim.names=TRUE){
    
    mf          <- as.mim.formula.object(mim.formula, mim.names=mim.names,
                                           names.table=data$names.table)
    mimModel.id <- paste("mimModel -",date(),"-",runif(1))
    value       <- list("formula"=mf, "mimData.id"=data$mimObject.id,
                        "mimObject.id"=mimModel.id,
                        "title"=data$title
                        )  
    class(value)<- "mimModel"
    assign(mimModel.id, value, env = .GlobalEnv)

    data$model.list <- c(data$model.list, mimModel.id)
    mimData.name    <- .find.mim.object(data$mimObject.id)
    ##cat("DATA OBJECT :", mimData.name, "\n")
    assign(mimData.name, data, env = .GlobalEnv)  
    return(value)
  }
  value <- make.mim.model.object(mim.formula, data, mim.names)
  if (submit==TRUE){
    submit.model(value, submit.data=submit.data)
    ##mim.cmd( paste("SetBlock; model ", value$formula$mim.formula,"; fit;"))
  }
  return(value)
}

mim.br.model <- function(mim.formula,data, mim.names=TRUE, submit=TRUE,
                         submit.data=TRUE){
  res <- 
    lapply(1:length(mim.formula),
           function(i){
             ##xx <- mim.model(mim.formula[i], data=data, submit=FALSE)
             xx <- mim.model(mim.formula[i], data=data, submit=FALSE,
                             mim.names=mim.names)
             print(xx)
           })
  mf <- as.mim.formula.object(block.list=res)

  mimBRModel.id   <- paste("mimBRModel -",date(),"-",runif(1))
  value           <- list("formula"=mf, "mimData.id"=data$mimObject.id,
                          "mimObject.id"=mimBRModel.id)  
  class(value)    <- "mimBRModel"
  assign(mimBRModel.id, value, env = .GlobalEnv)
  data$model.list <- c(data$model.list, mimBRModel.id)
  mimData.name    <- .find.mim.object(data$mimObject.id)
  assign(mimData.name, data, env = .GlobalEnv)  

  if (submit==TRUE){
    submit.model (value, submit.data=submit.data)
  }
  return(value)
}


submit.model     <- function(x,submit.data=TRUE)
  UseMethod("submit.model",x,submit.data=TRUE)

submit.model.mimModel <- function(x, submit.data=TRUE){
  mimModel <- x
  mimData <- .get.mim.object(.find.mim.object(mimModel$mimData.id))
  if (submit.data==TRUE){
    cat("Submitting data to MIM\n")
    submit.mimData(mimData)
    }
  cat("Submitting model to MIM\n")
  mim.cmd(print(paste("model ", mimModel$formula$mim.formula,"; fit;")))
}


submit.model.mimBRModel <- function(x, submit.data=TRUE){
  mimBRModel <- x
  mimData <- .get.mim.object(.find.mim.object(mimBRModel$mimData.id))
  if (submit.data==TRUE){
    cat("Submitting data to MIM\n")
    submit.mimData(mimData)
  }
  value <- mimBRModel
  cat("Submitting model to MIM\n")
  formula.list <- value$formula$block.list
  sss <-lapply(1:length(formula.list),
               function(i){
                 formula <- formula.list[[i]]$formula$mim.formula
                 print(paste("Model", print(formula), ";PutBlock", i, ";"))
               })
  sss <- paste( unlist(sss), collapse=' ')
  ##print(sss)
  mim.cmd(sss)
  
  ##mim.cmd(print(paste("model ", mimModel$formula$mim.formula,"; fit;")))
  
}


### mimModel
###


###
### mimData ###

as.mimData <- function(data, mim.names=NULL, mim.labels=NULL,
                     file=FALSE, submit=TRUE, n.digits=12, title=NULL ){
  mim.df    <- data
  fact.ind  <- rep(FALSE,dim(data)[2])
  cont.ind  <- rep(FALSE,dim(data)[2])
  ## use.names <- names(m);
  if (is.null(mim.labels))
    mim.labels <- rep(NA, length(names(data)))
  ##l         <- which(unlist(lapply(use.names,nchar))>1)
  l         <- which(unlist(lapply(names(data),nchar))>1)
  if (is.null(mim.names))
    if (length (l) > 1 )
      use.names <- c(letters,toupper(letters))[1:dim(data)[2]]
    else
      use.names <- names(data)
  else
    if (mim.names == "abc")
      use.names <- c(letters,toupper(letters))[1:dim(data)[2]]
    else{
      if (length(mim.names)>1)
        use.names <- mim.names
      else
        use.names <- partition.string.by(mim.names)
    }

  ##  print(fact.ind)
  fact.str <- NULL
  cont.str <- NULL
  for (j in 1:dim(data)[2]){
    fact.ind[j] <- is.factor(data[,j])
    cont.ind[j] <- !is.factor(data[,j])
    if (fact.ind[j]==TRUE){
      mim.df[,j]   <- codes(data[,j])
      fact.str <- c( fact.str, use.names[j], length(attr(data[,j],"levels")) )
    }
    else{
      cont.str <- c(cont.str, use.names[j]);
    }
  }
  ##print(fact.str); print(cont.str)
  
  names.obj <- NULL
  for(i in 1:length(names(data))){
    l1 <- list("names"=names(data)[i], "use.name"=use.names[i],
               "mim.labels"=mim.labels[i])
    l2 <- if (fact.ind[i]==TRUE)
      list("levels"=paste((attr(data[,i],"levels")),collapse=' '),
           "coding"=paste((1:length(attr(data[,i],"levels"))),collapse=' '))
    l3 <- c(l1,l2)
    names.obj <- c(names.obj, list(l3))
  }

  b <- lapply(names.obj, function(a1){
    c(a1$names,a1$use.name, a1$mim.labels, paste(a1$levels,collapse=" "),
      paste(a1$coding,collapse=" "))})

  names.df <- NULL;
  for (i in 1:length(b)){
    names.df <- rbind(names.df, b[[i]])
  }
  names.df <- as.data.frame(names.df)

  names(names.df) <-  c("df.names"  , "mim.names",
                        "mim.labels", "df.levels", "mim.levels")

  if (!is.null(mim.labels)){
    label.strings <- NULL
    for (i in 1:length(mim.labels)){
      lab   <- paste('"', mim.labels[i], '"',sep='');
      label <- paste("label ", use.names[i], lab, sep=' ')
      label.strings <- c(label.strings, list(label))
    }
  }

  ## Create unique name for object
  mimData.id <- paste("mimData -",date(),"-",runif(1))
  ##print(mimData.id)
  value  <- list("data"=data, "mim.df"=mim.df, 
                 "names.table"=names.df,
                 "fact.str"   =fact.str, "cont.str"=cont.str, 
                 "use.names"=use.names, "label.strings"=label.strings,
                 "model.list"=NULL, "title"=title, "mimObject.id"=mimData.id)
  class(value) <- "mimData"
  ##assign(mimData.id, value, env = .GlobalEnv)
  return(value)
}


submit.mimData <- function(mimData,file=NULL, submit=TRUE, n.digits=12){
  
  ## Create file
  if (is.null(file)){
    specfile <- tempfile(pattern="mimR_df2mim")
    file     <- paste(specfile,".txt",sep='')
  }
  else {
    if (length(grep(":", file))==0)
      file <- paste(getwd(),"\\",file,sep='')
  }

  cat("MIM data file: ", file, fill=TRUE)
  write("%\n% DATA FILE AUTOMATICALLY GENERATED BY mimR\n% ", file, append=FALSE)

  ## Write fact ...; cont ...;
  fact.cont.str  <-
    paste(
          if (!is.null(mimData$fact.str)) paste("fact", paste(mimData$fact.str, collapse=' '),";"),
          if (!is.null(mimData$cont.str)) paste("cont", paste(mimData$cont.str, collapse=' '),";")
          )
  ##cat(fact.cont.str, fill=TRUE)
  cat(fact.cont.str, file=file,append=TRUE,fill=TRUE)

  ## Write labels
  if (!is.null(mimData$mim.labels))
    for (i in 1:length(mimData$label.strings))
      cat(mimData$label.strings[[i]],file=file,append=TRUE,fill=TRUE)

  ## Write data
  names(mimData$mim.df) <- mimData$mim.names
  m2    <- t(mimData$mim.df)  
  write(append("read",mimData$use.names), file, ncolumns=50,append=TRUE)
  cat("Writing data",fill=FALSE)
  s3    <- proc.time()
  d     <- unlist( lapply( as.vector(m2), float.to.string,
                          n.digits=n.digits, width=15)) 
  write(d, file, append=TRUE, ncolumns=40)
  cat(" ... done. Time taken:",(proc.time()-s3)[3],fill=TRUE)
  write("!",file, append=TRUE)

  ## Writing file ... done
  if (submit==TRUE){
    ##str <- paste("clear; clear output;","input", file, ";", sep=' ');
    str <- paste("input", file, ";", sep=' '); 
    mim.cmd(str);
  }
}

print.mimData <- function(x, ...){
  cat(paste("mimObject.id:",x$mimObject.id),fill=TRUE)
  cat(paste("title       :",x$title),fill=TRUE)
  cat("names.table :\n")
  print(x$names.table)
  if (!is.null(x$model.list)){
    cat("model.list\n") 
    for (i in 1:length(x$model.list)){
      model.id  <- x$model.list[[i]] 
      model.obj <- .get.mim.object(model.id)
      cat("Model ", i,"\n")
      print(model.obj$formula, short=TRUE)
      
    }
  }
}
### mimData ###
###

get.models.mimData <- function(mimData,model=NULL,short=FALSE){

  print.formula <- function(mo,i=NULL){
    if (short==FALSE){
      cat("Model no:", i, fill=TRUE)
      print(mo$formula)
    }
    else{
      if (!is.null(i))
        cat("Model no:", i,  mo$formula$mim.formula, fill=TRUE)
      else
        print(mo$formula$mim.formula)
    }
  }

  if (!is.null(mimData$model.list)){
    model.id.list <- mimData$model.list
    value <- NULL;
    if (is.null(model)){
      for (i in 1:length(model.id.list)){
        mo <- .get.mim.object(model.id.list[[i]])
        cat("Model ", i,fill=TRUE)
        print(mo$formula, short=short)
        value <- c(value, list(mo))
      }
    }
    else{
      mo <- .get.mim.object(model.id.list[[model]])
      cat("Model ", model,fill=TRUE)
      print(mo$formula, short=short)
      value <- mo
    }
  }
}

get.models     <- function(mimData,model=NULL,short=FALSE)
  UseMethod("get.models",mimData,model,short)


###
### Miscellaneous

.get.mim.object <- function(object.name){
   value <- get (object.name, env = .GlobalEnv)
   return(value)
   }


.find.mim.object <- function(mimObject.id){
  object.list <- ls( env=.GlobalEnv)
  for (i in 1:length(object.list)){
    object.name <- object.list[i] 
    object <- get(object.name, env = .GlobalEnv)
    if (!is.null(class(object))){
      if (class(object)=="mimModel" || class(object)=="mimBRModel" ||
          class(object)=="mimData"){
          ##print(object.name)
          ##print(object$mimObject.id)
          if(mimObject.id == object$mimObject.id){
            ##print(mimObject.id)
            ##print(object$mimObject.id)
            return(object.name)
          } 
      }
    }
  }
}


### Miscellaneous
###















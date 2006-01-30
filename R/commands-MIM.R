
### Alt i denne fil skal senere slettes...

printMIM <- function(arg=NULL, verbose=FALSE){
  value <-
    switch(arg,
           s=,t=,u=,v=,f=,g=,h=,i= {
             res    <- mim.cmd( paste("print ", arg), look.nice=!verbose)
             val <- .get.mim.parameters(res)},
           w=,x=,y=,z=             {
             res    <- mim.cmd( paste("print ", arg), look.nice=!verbose)
             val <- paste( res,collapse=" ")},
           m=,b=," "=              {
             res    <- mim.cmd( paste("print ", arg), look.nice=!verbose)
             val <- paste(res, collapse=" ")},
           c=,d=,e =               {
             res    <- retrieveData(arg)           },    # [,-1]                  
           {print("DEFAULT")}
           )
  return(invisible(value))
}




##### .get.mim.parameters og .mim.mean.cov2df bruges kun af funktioner ovenfor i denne fil
##### og med tiden bør de nok fjernes.

".get.mim.parameters" <-
function(mim.output){
#
  mim.output.text  <- mim.output[is.na(.silent.as.numeric(mim.output))]
  ## Remove numbers from mim.output
  Means.i    <- c(which(mim.output.text=="Means"), which(mim.output.text=="Linear"))
  output.type<- ifelse ( length(which("Means"==mim.output.text))>0, "Count", "Discrete")

  start      <- max(c(which(mim.output.text=="parameters."     ), 
                      which(mim.output.text=="covariances."    ),
                      which(mim.output.text=="correlations."   ), 
                      which(mim.output.text=="algorithm)."     )))

  type.text              <- paste( mim.output[1:start], collapse= " ")
  working.mim.output     <- mim.output[-(1:start)]
  mim.output.text        <- mim.output.text[-(1:start)]

  disc.names <- NULL; cont.names <- NULL;
  
  first.Count.i <- c(which(mim.output.text=="Count")[1],which(mim.output.text=="Discrete")[1])
  first.Count.i <-  first.Count.i[which(!is.na(first.Count.i))]

  if ( length(Means.i)>0 ){
    first.Means.i <- c(which(mim.output.text=="Means")[1], which(mim.output.text=="Linear")[1])
    first.Means.i <-  first.Means.i[which(!is.na(first.Means.i))]
    cont.names   <- mim.output.text[(first.Means.i+1):(first.Count.i-1)]
    disc.and.cont.names <- mim.output.text[1:(first.Means.i-1)]
    disc.names   <-
      if (!setequal(disc.and.cont.names, cont.names))
        setdiff(disc.and.cont.names, cont.names)
      else
        NULL
  }
  else{
    disc.names   <- mim.output.text[1:(first.Count.i-1)]
  }

  disc.dim2 <- length(disc.names);   cont.dim2 <- length(cont.names); 
  
  if (!is.null(disc.names)){
    if (!is.null(cont.names)){
      ##cat("DISC AND CONT",fill=TRUE)
      res.item.list    <- NULL;
      wmo         <- working.mim.output[-(1:disc.dim2)]## Remove names of factors;
      Count.i     <- c(which(wmo=="Count"),which(wmo=="Discrete"))
      item.length <- Count.i[1]

      for (i in 1:length(Count.i)){
        curr.wmo <- wmo[1:item.length]
        wmo      <- wmo[-(1:item.length)]; ## Remove curr.wmo to prepare for next iteration
        curr.wmo.numbers <- curr.wmo[which(!is.na(.silent.as.numeric(curr.wmo)))]
        disc.levels <- curr.wmo.numbers[1:disc.dim2]
        mean.cov    <- .mim.mean.cov2df(curr.wmo.numbers[-(1:disc.dim2)], cont.names)
        means       <- mean.cov$mean;
        cov         <- mean.cov$cov
        count       <- as.numeric(curr.wmo.numbers[length(curr.wmo.numbers)])
        res         <- list("type.text"  =type.text,   "disc.names"=disc.names,
                            "disc.levels"=disc.levels, "cont.names"=cont.names,
                            "means"      =means,       "cov"=cov,
                            "counts"     =count,       "variable.type"="mixed",
                            "output.type"=output.type)
        res.item.list <- c(res.item.list , list(res))   
      }
      mmm <- NULL;
      for (i in 1:length(res.item.list))
        mmm <- rbind(mmm, .silent.as.numeric(res.item.list[[i]]$disc.levels))
      mmm<- apply(mmm,2,max)
      ##print(mmm)
      res.list <- list("type.text"=type.text, "stats"=res.item.list, "variable.type"="mixed",
                       "disc.levels"=mmm,"disc.names"=res.item.list[[1]]$disc.names,
                       "cont.names"=res.item.list[[1]]$cont.names
                       )
    }
    else{
      ##cat("DISC ONLY",fill=T)
      wmo      <- working.mim.output;
      m        <- as.data.frame(matrix(as.numeric(wmo[-(1:(length(disc.names)+1))]),
                                       ncol=(length(disc.names)+1),byrow=TRUE))
      names(m) <- c(disc.names, "counts")      
      res.list <- list("type.text"=type.text, "disc.names"=disc.names,
                       "table"    =m,         "variable.type"="discrete")
    }
  }
  else{
    ##cat("CONT ONLY",fill=T)
    wmo      <- working.mim.output;
    ##print(wmo)
    curr.wmo.numbers <- wmo[which(!is.na(.silent.as.numeric(wmo)))]
    ##print(curr.wmo.numbers)
    mean.cov    <- .mim.mean.cov2df(curr.wmo.numbers,cont.names)
    means       <- mean.cov$mean;
    cov         <- mean.cov$cov
    count       <- as.numeric(curr.wmo.numbers[length(curr.wmo.numbers)])
    ##print(mean.cov$mean);        print(mean.cov$cov); print(count)    
    res.list <- list("type.text" =type.text, "cont.names"=cont.names,
                     "means"     =means,     "cov"       =cov,
                     "counts"    =count,     "variable.type"="continuous",
                     "output.type"=output.type)
  }
  class(res.list) <- "mim.parameters"
  return(res.list)
}


".mim.mean.cov2df" <-
function(mimout,y.names,x.names=NULL){
  ##if (is.null(x.names)){ ## output from print <letter
  if (length(x.names)==0){ ## output from print <letter
    ##print("PRINT outout")
    y.dim <- length(y.names)
    m     <- matrix(0,nrow=(y.dim+1),ncol=y.dim)
    index <- 1
    for (i in 1:y.dim)
      for (j in 1:i){
        m[i,j] <- as.numeric(mimout[index])
        index  <- index + 1
      }
    for (j in 1:y.dim){
      m[(y.dim+1),j]<- as.numeric(mimout[index])
      index <- index + 1
    }
    #print(m)
    mean <- t(m[y.dim+1,1:y.dim, drop=FALSE])
    covm <- m[1:y.dim, 1:y.dim, drop=FALSE]
    ##names(mean) <- y.names
    ##print(mean)
    dimnames(mean) <- list( y.names, c("int"))
  }
  else{ ## output from display
    ##print("DISPLAY outout")
    m        <- matrix(0,nrow=length(y.names),ncol=1+length(x.names)+length(y.names))
    index    <- 1
    for (i in 1:length(y.names))
      for (j in 1:(1+length(x.names)+i)){
        m[i,j] <- as.numeric(mimout[index])
        index <- index + 1
      }
    mean  <- m[,  1:(1+length(x.names)) ,drop=FALSE]
    covm  <- m[,-(1:(1+length(x.names))),drop=FALSE]
    dimnames(mean) <- list( y.names, c("int", x.names))
  }

  if (nrow(covm)>1) covm <- as.data.frame( covm + t(covm) - diag(diag(covm)) )
  dimnames(covm) <- list( y.names, y.names )
  return(list("mean"=mean,"cov"=covm))
}


# displayMIM    <- function(y,x=NULL){
# ## Runs mim command display y,x
#   ep          <- TRUE;
#   sh.eprint   <- function(x)
#     {if (ep==TRUE)
#        {str <- deparse(substitute(x)); cat(paste("E>>", str," =", x),fill=TRUE)}}

#   y.list <- if (length(y)==1)
#       strsplit(y,"")[[1]]
#     else y
#   y.list <- y.list[order(y.list)]
  
#   x.list  <- if (!is.null(x))
#     if (length(x)==1)
#       strsplit(x,"")[[1]]
#     else x
#   if (!is.null(x.list))
#     x.list <- x.list[order(x.list)]

#   cmd <- paste("Display",paste(y,collapse=''), ",", paste(x,collapse='')); ##print(cmd)
#   mim.output <- mim.cmd(cmd, look.nice=FALSE);

#   if (length(grep("conditional", mim.output)) >0 || length(grep("marginal", mim.output)) >0){
#     is.cont.response <- TRUE;
#     is.disc.response <- FALSE;
#     start <- grep("\\.", mim.output)[2]
#   }
#   else{
#     is.cont.response <- FALSE;
#     is.disc.response <- TRUE;
#     start <- grep("\\.", mim.output)[1]
#   }
  
#   has.Count.in.output <- if (length(grep("Count", mim.output)) > 0) TRUE else FALSE
  
#   working.mim.output     <- mim.output[-(1:start)]

#   if (is.cont.response){
#     ##cat("CONT",fill=TRUE)
#     wmo <- working.mim.output;
#     first.resp.index <- grep(y.list[1], wmo)[1]
#     last.resp.index  <- grep(y.list[length(y.list)], wmo)[2]
#     factor.string    <- if (first.resp.index > 1)  wmo[1:(first.resp.index-1)] 
#     factor.names     <- if (!is.null(factor.string)) factor.string[  1:(length(factor.string)/2) ]

#     cont.x.names <- x.list;
#     if (!is.null(factor.string)){
#       first.resp.index <- first.resp.index - length (factor.names)
#       last.resp.index  <- last.resp.index  - length (factor.names)
#       if (has.Count.in.output ) last.resp.index <- last.resp.index + 1
#       cont.x.names     <- setdiff( x.list, factor.names )
#       wmo              <- wmo[-(1:length(factor.names))]
#     }

#     res.item.list <- NULL;
#     while ( (length(wmo) > 0) && (length(wmo)>=last.resp.index)){
#       curr.wmo <- wmo[1:last.resp.index]
#       factor.values    <- if (!is.null(factor.string)) curr.wmo[1:(length(factor.names))]
#       curr.wmo         <- curr.wmo[-(1:(length(factor.names)))]
#       curr.wmo.numbers <- curr.wmo[which(!is.na(.silent.as.numeric(curr.wmo)))]
#       mean.cov         <- .mim.mean.cov2df(curr.wmo.numbers, y.list, cont.x.names)
#       #print(mean.cov)
#       #print(y.list)
#       #print(cont.x.names)
      
#       means       <- mean.cov$mean;
#       cov         <- mean.cov$cov
#       res         <-
#         if(!is.null(factor.string))
#           list("disc.names"=factor.names, "disc.levels"=factor.values, "means"=means, "cov"=cov)
#         else
#           list("means"=means, "cov"=cov)

#       res.item.list <- c(res.item.list , list(res))   
#       wmo      <- wmo[-(1:last.resp.index)]
#     }
    
#     if (!has.Count.in.output)
#       type.text <- paste("Parameters of the conditional distribution of ",
#                          paste(y.list,collapse=","), "given ", paste(x.list,collapse=","))
#     else
#       type.text <- paste("Parameters of marginal distribution of ",
#                          paste(y.list,collapse=","))

#     if (!is.null(factor.string))
#        res.list <- list("type.text"=type.text, "stats"=res.item.list, "variable.type"="mixed")
#     else{
#       res.list <- list("type.text"=type.text, "means"=res.item.list[[1]]$means, "cov"=res.item.list[[1]]$cov,
#                        "variable.type"="continuous")
      
#     }
#   }
#   else{
#     ##cat("DISC",fill=T)
#     wmo <- working.mim.output;
#     ##print( wmo )
#     header <- wmo[which(is.na(.silent.as.numeric(wmo)))]
#     values <- wmo[which(!is.na(.silent.as.numeric(wmo)))]
#     ##print(header); print(values)
#     m <- as.data.frame(matrix(.silent.as.numeric(values), ncol=length(header),byrow=TRUE))
#     names(m) <- header;
#     ##print(m)
#     type.text <- paste("Linear predictors for  ",
#                        paste(y.list,collapse=","), "given ", paste(x.list,collapse=","))
    
#     res.list <- list("type.text"=type.text, table=m, "variable.type"="discrete")
#   }
#   class(res.list) <- "linpredictMIM"  
#   value <- res.list
#   return(value)
  
# }


as.mim.formula.object <- function(mim.formula=NULL, block.list=NULL, names.table=NULL, mim.names=TRUE){

  ##print(names.table)
  if (!is.null(block.list)){
    value <- list("block.list"=block.list)
    class(value) <- c("mim.block.recursive.model", "mim.model")
    if (!is.null(mim.formula))
      cat("WARNING: Possible inappropriate use of as.mim.formula.object\n")
  }
  else{
    value        <- parse.mim.formula(mim.formula=mim.formula, names.table=names.table,
                                      mim.names=mim.names)
    class(value) <- c("mim.undirected.model",      "mim.model")
  }
  ##print(value)
  return(invisible(value))
}

parse.mim.formula <- function(mim.formula, names.table=NULL,mim.names=TRUE){  

  mim.formula <- gsub("\n","",mim.formula)
  ##  if (!is.null(names.table)){
  if (mim.names==FALSE){
    ## Generate mim.formula with letters as names only
    a1 <- c(partition.string.by(mim.formula, "/"),NA,NA)[1:3]
    res <- lapply(a1,
                  function(x1){v<-look.up.mim.names(x1,names.table=names.table,
                                                    direction="to.mim")
                               v2 <- if (!is.na(v))
                                 paste(lapply(v,paste,collapse=''), collapse=',')
                               else
                                 NA
                             })
    
    if (is.na(res[[3]]))
      res <- paste(unlist(res[[1]]),'//')
    else
      if (is.na(res[[1]]))
        res <- paste('//',unlist(res[[3]]))
      else
        res <- paste(unlist(res),collapse='/')
    mim.formula <- res
  }

  ##print(mim.formula)
  a1 <- c(partition.string.by(mim.formula, "/"),NA,NA)[1:3]
  a2 <- lapply(a1, partition.string.by, ",")
##print(a2)  
  mim.discrete         <- a2[[1]];
  mim.linear           <- a2[[2]];
  mim.quadratic        <- a2[[3]];

  disc.mim.names <- ifelse( (!is.na(mim.discrete)),
    unique(unlist(lapply(mim.discrete, partition.string.by))), NA)
  l <- if (!is.na(mim.linear))
    unlist(lapply(mim.linear, partition.string.by))
  q <- if (!is.na(mim.quadratic))
    unlist(lapply(mim.quadratic, partition.string.by))

  all.mim.names <- if(!is.null(c(l,q)))
    unique(c(l,q))
  
  cont.mim.names <- setdiff(all.mim.names, disc.mim.names)
  cont.mim.names <- if (is.null(cont.mim.names)) NA else cont.mim.names

  value <- list("mim.formula"   =mim.formula,
                "mim.discrete"  =mim.discrete, "mim.linear"=mim.linear, "mim.quadratic"=mim.quadratic,
                "disc.mim.names"=disc.mim.names, "cont.mim.names"=cont.mim.names
                )

##  print(names.table)
  if (!is.null(names.table)){
    value$discrete  <-
      look.up.mim.names(mim.discrete,names.table=names.table,
                        direction="from.mim")
    value$linear    <-
      look.up.mim.names(mim.linear,names.table=names.table,
                        direction="from.mim")
    value$quadratic <-
      look.up.mim.names(mim.quadratic,names.table=names.table,
                        direction="from.mim")
    value$disc.names<-
      look.up.mim.names(disc.mim.names,names.table=names.table,
                        direction="from.mim")
    value$cont.names<-
      look.up.mim.names(cont.mim.names,names.table=names.table,
                        direction="from.mim")      
    value$with.names<- TRUE
  }
  else
    value$with.names<- FALSE
  return (value)
}


look.up.mim.names <- function(terms, names.table,direction="from.mim"){
  if (is.na(terms)) 
    return(NA)
  else{
    switch(direction,
           from.mim = {
             terms.list <- partition.string.by(terms, ",")             
             a <- lapply(terms.list, function(x1)
                         {
                           v <- partition.string.by(x1)
                           ##print(v)
                           v2<- paste(names.table$df.names[match(v, names.table$mim.names)])
                           
                         }
                         )
             
           },
           to.mim = {
             terms.list <- partition.string.by(terms, ",")             
             a <- lapply(terms.list, function(x1)
                         {
                           v <- partition.string.by(x1,":")
                           ##print(v)
                           v2<- paste(names.table$mim.names[match(v, names.table$df.names)])
                           
                         }
                         )

           }
           )
    return(a)
  }
}


create.table <- function(levels,names=NULL){
  f <- function(levels){
    if (length(levels)==1)
      value <- return(1:levels)
    else{
      x <- levels[1]
      rest <- levels[-1]
      rec.res <- f(rest)
      r2 <- NULL
      for (i in 1:x)
        r2 <- rbind(r2, cbind(i,rec.res))
      value <- r2
      
    }
    return(value)
  }
 value <- as.data.frame(f(levels))
    if (!is.null(names))
      names(value) <- names
    else
      names(value)    <-paste("x", 1:ncol(value),sep='')
    return(value)
  }




mim.mean.cov2df <- function(mimout,y.names,x.names=NULL){
  if (is.null(x.names)){ ## output from print <letter
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
    mean <- m[y.dim+1, ]
    covm <- m[1:y.dim, 1:y.dim, drop=FALSE]
    names(mean) <- y.names
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


silent.as.numeric <- function(string.vec) {
    unlist(lapply(string.vec,function(x){x2<-type.convert(x); 
     if(is.factor(x2)||is.logical(x2)||is.complex(x2)) NA else x2}))
}


float.to.string <- function(num.vec,n.digits=6,width=9, preserve.int=TRUE){
  a <- abs(num.vec)
  exp.indic <- regexpr("e",as.character(num.vec))
  zero.str  <- "00000000000000000000000000";
  if ( exp.indic < 0 && ((a-round(a))==0) && (preserve.int==TRUE) ){
    result <- paste(num.vec); #print("preserving int")
  }
  else{
    if (exp.indic > 0){
      sig <- sign(num.vec)
      pow.head    <-  substring(as.character(num.vec), 1, regexpr("e",as.character(num.vec))-1)
      int.part    <-  ifelse( (regexpr("\\.",pow.head) > 0 ),
                             substring(pow.head, 1, regexpr("\\.",pow.head)-1) ,   
                             pow.head)
      if(sig==-1) int.part <- substring(int.part,2)
      if(sig==-1) int.part <- substring(int.part,1)
      frac.part   <-  ifelse( (regexpr("\\.",pow.head) > 0 ),
                             substring(pow.head, regexpr("\\.",pow.head)+1),
                             "")
      pow.tail    <-  substring(as.character(num.vec), regexpr("e",as.character(num.vec))+1)
      if ((as.numeric(pow.tail)) < 0)
        zeros       <-  substring("0.0000000000000000",1,abs(round(as.numeric(pow.tail)))+1)
      else
        zeros       <-  substring("0.0000000000000000",1,abs(round(as.numeric(pow.tail)))+2)
      
      a2      <-  paste(zeros,int.part,frac.part,zero.str,sep='')
    }
    else{
      if ( (a-round(a))==0 ){
        a2 <-paste(a,".0000000000",sep='')}
      else{
        a2 <-paste(round(a,8),"0000000000",sep='')
      }
    }
    
    head <-  substring(a2, 1, regexpr("\\.",a2)-1)
    tail <-  substring(a2, regexpr("\\.",a2),regexpr("\\.",a2)+n.digits)
    if ( (num.vec-a)<0)
      result <- paste("-",head,tail,sep='')
    else
      result <- paste(" ",head,tail,sep='') 
    if(width > nchar(result)){
      for(i in 1:(width-nchar(result)))
        result <- paste(" ",result,sep='')
    }
  } 
  return(result)
}    

partition.string.by <- function(string, token=NULL){
  string <- gsub(' ', '', string)  ## spaces only

  value <- NULL;
  if (is.null(token)){
    value <- sapply(1:nchar(string), function(i) substr(string,i,i))
    return(value)
  }
  else{
    i <- regexpr(token, string)
    if (i==-1) 
      return(string)
    else{
      while( i != -1){
        sub.str <- substring(string,1,i-1) 
        if (sub.str=="") sub.str <- NA
        string  <- substring(string,i+1)
        i <- regexpr(token, string)    
        value <- c(value, sub.str)
      }
      if (nchar(string)>0)
        value <- c(value, string)
      return(value)
    }
  }
}


get.mim.parameters <- function(mim.output){
#
  mim.output.text  <- mim.output[is.na(silent.as.numeric(mim.output))]
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
        curr.wmo.numbers <- curr.wmo[which(!is.na(silent.as.numeric(curr.wmo)))]
        disc.levels <- curr.wmo.numbers[1:disc.dim2]
        mean.cov    <- mim.mean.cov2df(curr.wmo.numbers[-(1:disc.dim2)], cont.names)
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
        mmm <- rbind(mmm, silent.as.numeric(res.item.list[[i]]$disc.levels))
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
                                       ncol=(length(disc.names)+1),byrow=T))
      names(m) <- c(disc.names, "counts")      
      res.list <- list("type.text"=type.text, "disc.names"=disc.names,
                       "table"    =m,         "variable.type"="discrete")
    }
  }
  else{
    ##cat("CONT ONLY",fill=T)
    wmo      <- working.mim.output;
    ##print(wmo)
    curr.wmo.numbers <- wmo[which(!is.na(silent.as.numeric(wmo)))]
    ##print(curr.wmo.numbers)
    mean.cov    <- mim.mean.cov2df(curr.wmo.numbers,cont.names)
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



####  OBJECT METHODS
############################################################################

print.mim.parameters <- function(x,...){
  cat(x$variable.type, fill=TRUE)
  cat(x$type.text, fill=TRUE)
  switch(x$variable.type,
         discrete  ={
           print(x$table)},
         continuous={
           x.cov <- x$cov;
           x.mean<- x$means
           x.mean.cov <- as.data.frame(rbind(x.mean, x.cov))
           dimnames(x.mean.cov) <- list(c("means",paste("cov.",x$cont.names,sep='')),x$cont.names)
           if (!is.null(x$output.type))
             cat(paste(x$output.type, "=", x$counts),fill=TRUE)
           else
             cat(paste("Counts =", x$counts),fill=TRUE)
           print(x.mean.cov);
         },
         mixed     ={
           lapply(x$stats,
                  function(xx){
                    cat(paste(paste(xx$disc.names,collapse=","), "=",
                              paste(xx$disc.levels,collapse=","), sep='')," ")
                    x.cov <- xx$cov;
                    x.mean<- xx$means
                    x.mean.cov <- as.data.frame(rbind(x.mean, x.cov))
                    dimnames(x.mean.cov) <- list(c("means",paste("cov.",xx$cont.names,sep='')),
                                                 xx$cont.names)
                    if (!is.null(xx$output.type))
                      cat(paste(xx$output.type, "=", xx$counts),fill=TRUE)
                    else
                      cat(paste("Counts =", xx$counts),fill=TRUE)
                    print(x.mean.cov);
                  }
                  )
         }
         )   
  return(invisible(x))
}

print.mim.display <- function(x,...){
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

print.mim.model <- function(x, header=TRUE, short=FALSE, ...){

  print.mim.undirected.model <- function(x, ...){
    collapse <- function(a){
      a2<-lapply(a,function(b)paste(b,collapse=':'))
      a3<-paste(unlist(a2),collapse=', ')
      return(a3)
    }    
    cat("MIM formula:", x$mim.formula, fill=TRUE)
    if (short != TRUE){
      if (x$with.names==FALSE){
        if( !is.na(x$mim.discrete) )
          cat(paste(" Discrete :",collapse(x$mim.discrete)), fill=TRUE)
        if( !is.na(x$mim.linear) )
          cat(paste(" Linear   :",collapse(x$mim.linear)),   fill=TRUE)
        if( !is.na(x$mim.quadratic) )
          cat(paste(" Quadratic:",collapse(x$mim.quadratic)),fill=TRUE)    

      }
      else{
        if( !is.na(x$discrete) )
          cat(paste(" Discrete :",collapse(x$discrete)), fill=TRUE)
        if( !is.na(x$linear) )
          cat(paste(" Linear   :",collapse(x$linear)),   fill=TRUE)
        if( !is.na(x$quadratic) )
          cat(paste(" Quadratic:",collapse(x$quadratic)),fill=TRUE)    
      }
    }
    return(x)
  }
  
  print.mim.block.recursive.model <- function(x, ...){
    block.list <- x$block.list
    for (i in 1:length(block.list)){
      cat("Block:", i, collapse=' ',fill=FALSE)
      print(block.list[[i]]$formula)
    }
    return(x)
  }

  if (short != TRUE){
    if (header)
      if (class(x)[1]=="mim.block.recursive.model")
        cat("Model type: Block recursive model\n")
      else
        cat("Model type: Undirected model\n")
  }
  if (class(x)[1]=="mim.block.recursive.model")
    print.mim.block.recursive.model(x)
  else
    print.mim.undirected.model(x)

  value <- x
  return(invisible(x))
  
}
  

############################################################################
####  OBJECT METHODS

mim.setblock <- function(br.structure=NULL,data=NULL,mim.names=TRUE){
  if (is.null(br.structure)){
    value <- paste("SetBlock")
    mim.cmd(value)
  }
  else{
    if (!is.null(data)){
      if (length(br.structure)>1)
        br.structure <- paste(br.structure, collapse=' | ')
      ##print(br.structure)
      if (mim.names==FALSE){
        names.table <- data$names.table
        last.token <- ifelse (is.null(names.table), "", ",") 
        s1 <- lapply(partition.string.by(br.structure, "\\|"),
                     partition.string.by, last.token)
        s2 <- lapply(s1, function(a){
          b <- look.up.mim.names(a,names.table=names.table, direction="to.mim")
          paste(as.vector(b),collapse='')}) 
        s3 <- paste(s2, collapse="|")
      }
      else{
        s3 <- br.structure
      }
      value <- paste("SetBlock", s3)        
      mim.cmd(value)
    }
    else
      value <- NULL
  }
  print(value)
  return(invisible(value))
}



save.mimData <- function(x, file="mimData"){
    a <-  print(c(paste(substitute(x)), x$model.list))
    ##print(a)
    save(list=a, file=file)

}






.look.up.mim.names <- function(terms, names.table,direction="from.mim"){
  if (length(terms)==0)
    return(NULL)
  else
    if (is.list(terms))
      return(lapply(terms, .look.up.mim.names, names.table=names.table, direction=direction))
    else {
      if (length(terms)>1){
          return(unlist(lapply(terms, .look.up.mim.names, names.table=names.table, direction=direction)))
        }
      else
        if (is.na(terms)) 
          return(NA)
        else{
          terms.list <- .partition.string.by(terms, ",")                              
          switch(direction,
                 from.mim = {
                   a <- lapply(terms.list, function(x1){
                     v <- .partition.string.by(x1)
                     v2<- paste(names.table$name[match(v, names.table$letter)])
                   }
                               )
                   
                 },
                 to.mim = {
                   a <- lapply(terms.list, function(x1){
                     v <- .partition.string.by(x1,":")
                     v2<- paste(names.table$letter[match(v, names.table$name)])
                     v2
                   }
                               )
                   
                 }
                 )
                                        #print(a)
          return(unlist(a))
        }
    }
  }

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


".mim.setblock" <-
function(br.structure=NULL,data=NULL,mim.names=FALSE){
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
        s1 <- lapply(.partition.string.by(br.structure, "\\|"),
                     .partition.string.by, last.token)
        s2 <- lapply(s1, function(a){
          b <- .look.up.mim.names(a,names.table=names.table, direction="to.mim")
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




".print.mim.model" <-
function(x, header=TRUE, short=FALSE, ...){

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




".print.mim.parameters" <-
function(x,...){
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
                    x.mean.cov <- as.data.frame(rbind(t(x.mean), x.cov))

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



is.discrete <- function(mim){
  used <- .used.names(mim)
  d <- mim$data
  a <- match(used,d$name)
  v <-all(d$factor[a])
  return(v)
}

is.continuous <- function(mim){
  used <- .used.names(mim)
  d <- mim$data
  a <- match(used,d$name)
  v <-all(!d$factor[a])
  return(v)
}

variableType <- function(mim){
  if (is.discrete(mim))
    value <- "discrete"
  else
    if (is.continuous(mim))
      value <- "continuous"
    else
      value <- "mixed"
  return(value)
}

.used.names <- function(mim,letter=FALSE){
  value <- unique(unlist(mim$modelInfo$Formula.as.list))
  if (letter==FALSE)
    value <- .look.up.mim.names(value, mim$data, "from.mim")
  return(value)
}

.latent.in.model <- function(mim){
  s <- mim$data
  used.names <- .look.up.mim.names(unique(unlist(mim$modelInfo$Formula.as.list)), s,"from.mim")
  v <- intersect(used.names, latent(s))
  value<-if (length(v)>0) v
  return(value)
}


.partition.mim.input <- function(input,token=NULL){    
  curr     <- input
  n.char   <- 50 
  res <- NULL
    while(sum(nchar(curr))>n.char){
    cs <- cumsum(nchar(curr)+1)
    res <- c(res, paste(curr[cs<=n.char], collapse=' '))
    curr <- curr[!(cs<=n.char)]
  }
  value <- c(res, paste(curr, collapse=' '))
  return(value)
}


###
### Miscellaneous stuff for mimR ver 1.01 (with gR-inspiration)
###

.q.by <- function(fvobj){
  q       <- fvobj$quadratic
  if (!is.null(q)){
    qt      <- as.data.frame.table(q)
    d.names <- names(dimnames(q)[-c(1,2)])
    c.names <- as.vector(dimnames(q)[[1]])
    q.by    <- by(qt, as.list(qt[,d.names,drop=FALSE]), function(a) a$Freq)
    return(q.by)
  }
}

.l.by <- function(fvobj){
  l <- fvobj$linear
  if (!is.null(l)){
    lt <- as.data.frame.table(l)
    d.names <- names(dimnames(l)[-c(1)])
    c.names <- as.vector(dimnames(l)[[1]])
    l.by       <- by(lt, as.list(lt[,d.names, drop=FALSE]), function(a) a$Freq)
    return(l.by)
  }
}

.d.by <- function(fvobj){
  d <- fvobj$discrete
  dt       <- as.data.frame.table(d)
  d.names  <- if (!is.vector(d)) names(dt)[1:(ncol(dt)-1)]
  d.by     <- by(dt, as.list(dt[,d.names,drop=FALSE]), function(a) a$Freq)
  return(d.by)
}

.d.names <- function(fvobj){
  d <- fvobj$discrete
  dt       <- as.data.frame.table(d)
  d.names  <- if (!is.vector(d)) names(dt)[1:(ncol(dt)-1)]
  return(d.names)
}

.d.levels <- function(fvobj){
  d <- fvobj$discrete
  d.levels <- if (!is.vector(d)) sapply(dimnames(d),length)
  return(d.levels)
}

.c.names <- function(fvobj){
  l <- fvobj$linear
  if (!is.null(l)){
    lt <- as.data.frame.table(l)
    c.names <- as.vector(dimnames(l)[[1]])
    return(c.names)
  }
}









############# NEW ###################

.returnValuesPrimitive <- function(object, type, marginal=NULL){
  value <- switch(type,
                  "fitted"={
                    .generic.FittedValues(object$modelInfo$FittedValues,
                            object$modelInfo$Homogeneous)},
                  "obs"=,"observed"=,"counts"={
                    .generic.FittedValues(object$suffStats)},
                  )
  
  if (!is.null(marginal)){
    if (variableType(object)=="discrete"){
      marginal <- gsub(' +','', marginal)
      marginal <- unlist(strsplit( marginal, '[ +: +]'))
      str   <- formula(paste("Freq ~ ",paste(marginal,collapse='+')))
      value <- as.data.frame(xtabs(str, data=value))
    } else {
      stop("Marginalization only implemented for discrete models")
    }
  }
  return(value)
}

#                valueType  
# variableType   observed   fitted   deviance  pearson   (+ marginal)
# disc              X          X        X         X
# cont              X          X
# mix               X          X


returnValues <- function(object,type="fitted",marginal=NULL){
  log0 <- function(x){
    sapply(x,function(a)ifelse(a==0,0,log(a)))
  }
  div <- function(a,b){
    v <- a/b
    v[b==0]<-0
    return(v)
  }
  
  value <-
    switch(type,
           "fitted"=,"observed"=,"obs"={
             value <- .returnValuesPrimitive(object,type,marginal)
           },
           "deviance"=,"pearson"={
             if (variableType(object)=="discrete"){
               valo <- .returnValuesPrimitive(object,"observed",marginal)
               vale <- .returnValuesPrimitive(object,"fitted",marginal)
               ##print(cbind(valo,vale))
               o <- valo$Freq
               e <- vale$Freq
               ##print(o); print(e)
               switch(type,
                      "deviance"={
                        d <- -2 * o * log0( div(e,o) )},
                      "pearson"={
                        d <- div((o-e)^2,e) }
                      )
               valo$Freq <- d
               valo
             }else{
               stop("Deviance only implemented for discrete models")
             }
           }
           )
  return(value)
}


fitted.mim <- function(object, ...){
  fv      <- object$modelInfo$FittedValues
  is.homo <- object$modelInfo$Homogeneous
  v <- .generic.FittedValues(fv,is.homo)
  return(v)
}


.generic.FittedValues <- function(fv, is.homo=NULL){
  if (is.null(is.homo))
    is.homo <- FALSE

  dd <- .d.by(fv)
  ll <- .l.by(fv)
  qq <- .q.by(fv)

  counts <- as.data.frame(as.vector(dd))
  names(counts) <- "Freq"
  
  if (length(dd)>1){
    tab <- create.table(.d.levels(fv), .d.names(fv))
    value <- cbind(tab, counts)
  } else {
    value <- counts
  }
  if (!is.null(ll)){
    means<-as.data.frame(matrix(unlist(ll),
                                ncol=length(.c.names(fv)), byrow=TRUE))
    names(means) <- .c.names(fv) 
    covm <- matrix(unlist(qq), ncol=length(.c.names(fv))^2, byrow=TRUE)
    
    if (is.homo==TRUE){
      v <- NULL
      for (i in 1:nrow(means))
        v <- rbind(v, covm)
      covm <- v
    }
    
    covariances <- as.data.frame(covm)
    names(covariances)<-
      unlist(lapply(.c.names(fv),
                    function(x)paste(x,":", .c.names(fv),sep='')))
    value <- cbind(value, means, covariances)    
  }
  return(value)
}


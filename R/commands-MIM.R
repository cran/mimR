
retrieveData <- function(arg="d", impute=FALSE){
  if (impute==TRUE)
    mim.cmd("impute")
  value<- .rsprint(arg)

  names(value$Data) <- value$Variables$name
  value <- value$Data
  return(value)
}

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


.testdeleteMIM <- function(edge,options=NULL){
  mim.out <- mim.cmd(paste("testdelete ", paste(edge, collapse=''),
                           options), look.nice=TRUE)
  test.type.index <- min(unlist(sapply( c("LR:", "F:"), grep,  mim.out)))
  value <- paste(mim.out[-(1:(test.type.index-1))], collapse=' ') 
  return(invisible(value))
}

.fitMIM <- function (){
  mim.output <- mim.cmd("fit", look.nice=FALSE)
  res <- .silent.as.numeric( mim.output[c(2,4)] )
  value <- c("deviance"=res[1], "df"=round(res[2],0))
  return(print(value))
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


displayMIM    <- function(y,x=NULL){
## Runs mim command display y,x
  ep          <- TRUE;
  sh.eprint   <- function(x)
    {if (ep==TRUE)
       {str <- deparse(substitute(x)); cat(paste("E>>", str," =", x),fill=TRUE)}}

  y.list <- if (length(y)==1)
      strsplit(y,"")[[1]]
    else y
  y.list <- y.list[order(y.list)]
  
  x.list  <- if (!is.null(x))
    if (length(x)==1)
      strsplit(x,"")[[1]]
    else x
  if (!is.null(x.list))
    x.list <- x.list[order(x.list)]

  cmd <- paste("Display",paste(y,collapse=''), ",", paste(x,collapse='')); ##print(cmd)
  mim.output <- mim.cmd(cmd, look.nice=FALSE);

  if (length(grep("conditional", mim.output)) >0 || length(grep("marginal", mim.output)) >0){
    is.cont.response <- TRUE;
    is.disc.response <- FALSE;
    start <- grep("\\.", mim.output)[2]
  }
  else{
    is.cont.response <- FALSE;
    is.disc.response <- TRUE;
    start <- grep("\\.", mim.output)[1]
  }
  
  has.Count.in.output <- if (length(grep("Count", mim.output)) > 0) TRUE else FALSE
  
  working.mim.output     <- mim.output[-(1:start)]

  if (is.cont.response){
    ##cat("CONT",fill=TRUE)
    wmo <- working.mim.output;
    first.resp.index <- grep(y.list[1], wmo)[1]
    last.resp.index  <- grep(y.list[length(y.list)], wmo)[2]
    factor.string    <- if (first.resp.index > 1)  wmo[1:(first.resp.index-1)] 
    factor.names     <- if (!is.null(factor.string)) factor.string[  1:(length(factor.string)/2) ]


    cont.x.names <- x.list;
    if (!is.null(factor.string)){
      first.resp.index <- first.resp.index - length (factor.names)
      last.resp.index  <- last.resp.index  - length (factor.names)
      if (has.Count.in.output ) last.resp.index <- last.resp.index + 1
      cont.x.names     <- setdiff( x.list, factor.names )
      wmo              <- wmo[-(1:length(factor.names))]
    }

    res.item.list <- NULL;
    while ( (length(wmo) > 0) && (length(wmo)>=last.resp.index)){
      curr.wmo <- wmo[1:last.resp.index]
      factor.values    <- if (!is.null(factor.string)) curr.wmo[1:(length(factor.names))]
      curr.wmo         <- curr.wmo[-(1:(length(factor.names)))]
      curr.wmo.numbers <- curr.wmo[which(!is.na(.silent.as.numeric(curr.wmo)))]
      mean.cov         <- .mim.mean.cov2df(curr.wmo.numbers, y.list, cont.x.names)
      #print(mean.cov)
      #print(y.list)
      #print(cont.x.names)
      
      means       <- mean.cov$mean;
      cov         <- mean.cov$cov
      res         <-
        if(!is.null(factor.string))
          list("disc.names"=factor.names, "disc.levels"=factor.values, "means"=means, "cov"=cov)
        else
          list("means"=means, "cov"=cov)

      res.item.list <- c(res.item.list , list(res))   
      wmo      <- wmo[-(1:last.resp.index)]
    }
    
    if (!has.Count.in.output)
      type.text <- paste("Parameters of the conditional distribution of ",
                         paste(y.list,collapse=","), "given ", paste(x.list,collapse=","))
    else
      type.text <- paste("Parameters of marginal distribution of ",
                         paste(y.list,collapse=","))

    if (!is.null(factor.string))
       res.list <- list("type.text"=type.text, "stats"=res.item.list, "variable.type"="mixed")
    else{
      res.list <- list("type.text"=type.text, "means"=res.item.list[[1]]$means, "cov"=res.item.list[[1]]$cov,
                       "variable.type"="continuous")
      
    }
  }
  else{
    ##cat("DISC",fill=T)
    wmo <- working.mim.output;
    ##print( wmo )
    header <- wmo[which(is.na(.silent.as.numeric(wmo)))]
    values <- wmo[which(!is.na(.silent.as.numeric(wmo)))]
    ##print(header); print(values)
    m <- as.data.frame(matrix(.silent.as.numeric(values), ncol=length(header),byrow=TRUE))
    names(m) <- header;
    ##print(m)
    type.text <- paste("Linear predictors for  ",
                       paste(y.list,collapse=","), "given ", paste(x.list,collapse=","))
    
    res.list <- list("type.text"=type.text, table=m, "variable.type"="discrete")
  }
  class(res.list) <- "linpredictMIM"  
  value <- res.list
  return(value)
  
}





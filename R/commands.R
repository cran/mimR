

mim.print <- function(arg=NULL, verbose=FALSE){

  ep          <- TRUE;
  sh.eprint   <- function(x)
    {if (ep==TRUE)
       {str <- deparse(substitute(x)); cat(paste("E>>", str," =", x),fill=TRUE)}}
  
  get.mim.data <- function( mim.output ){
    names         <- mim.output[ which(is.na(silent.as.numeric( mim.output )) ) ]    
    mim.output[ mim.output=="*" ] <- NA
    mim.output    <- mim.output[-(1:length(names))]
    result        <- as.data.frame(matrix(silent.as.numeric( mim.output ),
                                          ncol=length(names), byrow=T))
    names(result) <- names
    result
  }

  find.model.type <- function(res.str){
    a <- lapply(c("No model is currently defined.",
                  "No block-recursive model is currently defined.",
                  "The current model", "The current block-recursive model is:"),
                function(a) length(grep(a,res.str)))
    a <- which(unlist(a)>0)
    v <- switch(a, "1"="no.model","2"="no.model","3"="undirected", "4"="block.recursive")
    return(v)
  }


  
  if (is.null(arg) || length(which(!is.na(match( arg, c("m", "b") )))) > 0){

    res    <- mim.cmd( paste("print ", arg), look.nice=!verbose)

    res.str<- paste(res, collapse=" ")
    model.type <- find.model.type(res.str)
    switch(model.type,
           "no.model"  = {value  <- res.str},   
           "undirected"= {
             value <- as.mim.formula.object(mim.formula=res[5]);
 ##            value <- make.mim.model(mim.formula=res[5], is.parsed=FALSE);
           },
           "block.recursive" = {
             i      <- grep("is:", res)
             r      <- res[-(1:i)]
             block.list  <- NULL
             for (b in 2*1:(length(r)/2)){ 
               item <- as.mim.formula.object(mim.formula=r[b]);
##               item <- make.mim.model(mim.formula=r[b], is.parsed=FALSE);
               block.list <- c(block.list, list(item))
             }
##             value <- make.mim.model("block.list"=block.list,"is.parsed"=FALSE)
             value <- as.mim.formula.object("block.list"=block.list)
           }
           )
  }
  else
    {
      res <- mim.cmd( paste("print ", arg), look.nice=FALSE)
      arg <- tolower(arg)
      if (length(which(!is.na(match( arg, c("s","t","u","v","f","g","h","i") )))) > 0)
        value <- get.mim.parameters(res)
      if (length(which(!is.na(match( arg, c("w") )))) > 0)
        value <- res;
      if (length(which(!is.na(match( arg, c("x", "y", "z") )))) > 0)
        value <- res;
      if (length(which(!is.na(match( arg, c("c", "d", "e") )))) > 0)
        value <- get.mim.data( res )[,-1];
    }
  if (verbose)
    return(invisible(value))
  else
    return(value)
}



mim.display    <- function(y,x=NULL){
## Runs mim command display y,x

  ep          <- TRUE;
  sh.eprint   <- function(x)
    {if (ep==TRUE)
       {str <- deparse(substitute(x)); cat(paste("E>>", str," =", x),fill=TRUE)}}

  y.list  <- (strsplit(y,"")[[1]]);
  len.y   <- length(y.list);
  
  x.list  <- if (!is.null(x))(strsplit(x,"")[[1]]);
  len.x   <- length(x.list); 
  
  mim.output <- mim.cmd(paste("Display",y, ",", x), look.nice=FALSE);
  
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
      curr.wmo.numbers <- curr.wmo[which(!is.na(silent.as.numeric(curr.wmo)))]      
      mean.cov         <- mim.mean.cov2df(curr.wmo.numbers, y.list, cont.x.names)

      means       <- mean.cov$mean;
      cov         <- mean.cov$cov
      res         <-
        if(!is.null(factor.string))
          list("disc.names"=factor.names, "disc.levels"=factor.values, "means"=means, "cov"=cov)
        else
          list("means"=means, "cov"=cov)

      res.item.list <- c(res.item.list , list(res))   
      ##print(wmo)
      ##print(last.resp.index)
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
    header <- wmo[which(is.na(silent.as.numeric(wmo)))]
    values <- wmo[which(!is.na(silent.as.numeric(wmo)))]
    ##print(header); print(values)
    m <- as.data.frame(matrix(silent.as.numeric(values), ncol=length(header),byrow=TRUE))
    names(m) <- header;
    ##print(m)
    type.text <- paste("Linear predictors for  ",
                       paste(y.list,collapse=","), "given ", paste(x.list,collapse=","))
    
    res.list <- list("type.text"=type.text, table=m, "variable.type"="discrete")
  }
  class(res.list) <- "mim.display"  
  value <- res.list
  return(value)
  
}


mim.fit <- function (){
## Returns the deviance and df. To be used with MIM command fit  
  mim.output <- mim.cmd("fit", look.nice=FALSE)
  res <- silent.as.numeric( mim.output[c(2,4)] )
  value <- c("deviance"=res[1], "df"=round(res[2],0))
  return(print(value))
}

mim.emfit <- function(arg="R",plot=FALSE){
## Runs EMfit with option arg (either R, S or F) and returns data frame 
## with iteration history. If plot is T the iteration history is plotted

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

    
mim.testdelete <- function(edge,options=NULL){
  mim.out <- mim.cmd(paste("testdelete ", paste(edge, collapse=''), paste(options)), look.nice=TRUE)
  test.type.index <- min(unlist(sapply( c("LR:", "F:"), grep,  mim.out)))
  value <- paste(mim.out[-(1:(test.type.index-1))], collapse=' ') 
  return(invisible(value))
}

mim.stepwise <- function(options=NULL,short=FALSE){
  mim.out <- mim.cmd(paste("stepwise ", options, collapse=''), look.nice=!short)
  index   <- min(grep("Selected", mim.out))
  if (short){
    value.str <- paste( mim.out[-(1:(index-1))], collapse=' ')
    cat(value.str,fill=TRUE)
  }
  value     <- mim.out[-(1:(index+1))]
  return(invisible(value))
}







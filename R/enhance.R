
mim.read <- function(data, mim.names=NULL, mim.labels=NULL,
                     file=NULL, submit=TRUE, n.digits=12,title=NULL )
{
  value <- as.mimData(data, mim.names=mim.names, mim.labels=mim.labels,
                      title=title)
  submit.mimData(value, file=file, submit=submit, n.digits=n.digits)
  return(value)
}



mim.statread <- function(stat){
  
  if (length(class(stat)!="mim.parameters")==0){
    cat("Error: Argument to mim.statread is not of type mim.parameters\n")
    return();
  }
  
  
  extract.lower.tri <- function(m2){
    m2[upper.tri(m2)] <- NA
    value <- as.vector(m2)[which(as.vector(m2)!="NA")]
    return(value)
  }
  
  str1 <- paste("fact", paste(paste(stat$disc.names, stat$disc.levels),collapse = " "))
  str2 <- paste("cont", paste(stat$cont.names,collapse=' '))
  str3 <- paste("statread", paste(stat$disc.names,stat$cont.names,collapse=' '))   
  
  if (is.null(stat$cont.names))
    str4 <- paste(paste(stat$table$counts,collapse=' '),"!")
  else
    if (is.null(stat$disc.names))
      str4 <- paste(paste(stat$counts), paste(stat$means,collapse=' '),
                    paste(extract.lower.tri(stat$cov),collapse=' '),"!")
    else{
      str4 <- 
        lapply(stat$stats,
               function(a){
                 paste(paste(a$counts), paste(a$means,collapse=' '),
                 paste(extract.lower.tri(a$cov),collapse=' '))
               }
               )
      str4 <- paste(paste(str4, collapse=' '),"!")
    }
  mim.cmd(paste(str1,";",str2,";",str3))
  mim.cmd(str4)
##  print(c(str1,str2,str3,str4))
}


make.mim.stats <- function(disc.names=NULL, cont.names=NULL,  disc.levels=NULL, 
                            means=NULL, cov=NULL, counts=NULL, stats=NULL, table=NULL){

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
  
  if (is.null(cont.names) & !is.null(disc.names))
    variable.type <- "discrete"
  else 
    if (is.null(disc.names) & !is.null(cont.names))
      variable.type <- "continuous"
    else{
      if (!is.null(disc.names) & !is.null(cont.names))
        variable.type <- "mixed"
      else
        error()
    }

  switch(variable.type,
         discrete   = {r <- create.table(disc.levels, names=disc.names)
                       r$counts <- counts
                       value <- list("variable.type"="discrete", "disc.names"=disc.names,
                                     "disc.levels"=disc.levels, "table"=r)
                     },
         continuous = {
           m <- means; names(m) <- cont.names;
           s <- cov;dimnames(s) <- list(cont.names,cont.names)           
           value <- list("variable.type"="continuous", "cont.names"=cont.names,
                                     "means"=m, "cov"=s, "counts"=counts)
                     },
         mixed      = {r <- create.table(disc.levels,names=disc.names)
                       if (length(cov)==1) cov <- rep(cov, length(means))
                       stats <- lapply(1:length(means),
                                       function(i){
                                         m <- means[[i]]; names(m) <- cont.names;
                                         s <- cov[[i]];dimnames(s) <- list(cont.names,cont.names)
                                         list("means"=m,"cov"=s,
                                              "disc.names"=disc.names, "cont.names"=cont.names,
                                              "disc.levels"=unlist(r[i,]),counts=counts[i])  
                                       })
                       value <- list("variable.type"="mixed",  "disc.names"=disc.names,
                                     "cont.names"=cont.names, "disc.levels"=disc.levels,
                                     "stats"=stats)
                     }
         )
  
  value$type.text <- "specification from R"
  class( value )  <- "mim.parameters"
  return( value )
}




mim.diary.data <- function(file=NULL){
#
# Dumps data from MIM onto an ascii file
#

  if (is.null(file)){
    specfile <- tempfile(pattern="mimR_diary")
    file <- paste(specfile,".txt",sep='')
  }
  cat("Diary file : ",file, sep=' ', fill=TRUE)
  mim.cmd(paste("clear o; diaryon ", file, "; print d; diaryoff"))
  result <- read.table( file=file, header=TRUE, na.strings="*")
  return( result[,-1] )
}



#### Is DOCed above here
























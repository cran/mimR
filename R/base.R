
require(MASS)

.First.lib <- function(lib, pkg)
{
  ##print(lib); print(pkg)
  cat("\n")
  cat("-------------------------------------------------------------\n")
  cat(package.description("mimR", lib = lib, field="Title"))
  cat("\n")
  ver  <- package.description("mimR", lib = lib, field="Version")
  maint<- package.description("mimR", lib = lib, field="Maintainer")
  built<- package.description("mimR", lib = lib, field="Built")
  URL  <- package.description("mimR", lib = lib, field="URL")
  cat(paste("mimR, version", ver,  "is now loaded\n"))
  cat("Copyright (C) 2002, Søren Højsgaard\n")
  cat("Maintained by",maint,"\n")
  cat("Webpage:",URL,"\n")
  cat("\nBuilt:",built,"\n")
  cat("NOTICE:\n")
  cat("o mimR is available on Windows platforms only \n")
  cat("o To use mimR the MIM program must be running\n")
  cat("o The current version of mimR requires MIM version 3.1.2.20 or later\n")
  cat("o MIM is available from http://www.hypergraph.dk.\n")
  cat("o The current version of mimR requires R version 1.7.1 or later\n")
  #cat("\n  For a demo of mimR, type demo(mimR)\n")
  cat("-------------------------------------------------------------\n")
  return(invisible(0))
}

.Last.lib <- function(lib) {
  cat("Thank you for using mimR\n")
  return(invisible(0))
}

mim.cmd <- function(cmd, look.nice = TRUE, return.look.nice=FALSE, version='R') {
  if (!is.character(cmd)) stop("invalid input")
  outLines <- character(0)
  if (version=='R') {
    MIM <- socketConnection(port=50) 
    for (i in 1:length(cmd)) {
      theseLines <- character(0)
      writeLines(cmd[i], MIM, sep='')
      repeat {
        theseLines <- c(theseLines, readLines(MIM))
        if (length(theseLines) == 0) next
        if (theseLines[length(theseLines)] == 'COMPLETE') break }
      outLines <- c(outLines, theseLines[-length(theseLines)])
      if (look.nice==TRUE)    
        sapply(outLines,function(x) cat(x, fill=TRUE))
      if (return.look.nice==TRUE){
        value <- outLines
      }
      else{
        str2 <- paste(outLines, collapse = " ")
        value <- unlist(strsplit(str2, " +"))
        value <- value[value!=""]    
      }
    }
    close(MIM)
    return( invisible(value) );
  } else {
    MIM <- create.ole.object("mim31.Server")
    for (i in 1:length(cmd)) {
      NoOutputLines <- call.ole.method(MIM, "SendCmdLine", cmd[i]) 
      for (i in 1:NoOutputLines)
        outLines <- c(outLines,
                      call.ole.method(MIM, "GetOutputLine")) }
    release.ole.object(MIM)
    return(outLines)
  }
}  



mcm <- function(){
  cat("Enter MIM commands here. Type quit to return to R\n")
  x <- readline("MIM->")
  while(length(which(c("stop","end","quit","exit","e","q")==x))==0){
    mim.out <- mim.cmd(x,look.nice=TRUE)
    x <- readline("MIM->")
  }
}

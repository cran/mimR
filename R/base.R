.First.lib <- function(lib, pkg)
{
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
  cat("o To use mimR the MIM program must be installed on your\n")
  cat("  computer (Windows only)\n")
  cat("o The current version of mimR requires MIM version 3.1.2.9 or later\n")
  cat("o MIM (including a free student version and free upgrades)\n")
  cat("  is available from http://www.hypergraph.dk.\n")
  cat("o The executable mimBatch.exe (which comes with the mimR package)\n")
  cat("  must be placed somewhere on your path.\n")
  cat("o Before starting using mimR, make sure that MIM is \n")
  cat("  running.\n")
  cat("\n  For a demo of mimR, type demo(mimR)\n")
  cat("-------------------------------------------------------------\n")
  return(invisible(0))
}

.Last.lib <- function(lib) {
  cat("Thank you for using mimR\n")
  return(invisible(0))
}


mim.cmd <- function(mim.cmds, look.nice=TRUE){
  ##  Writes a specfile with MIM commands, submits this to MIM and 
  ##  returns the output

  ep          <- TRUE;
  sh.eprint   <- function(x)
    {if (ep==TRUE)
       {str <- deparse(substitute(x)); cat(paste("E>>", str," =", x),fill=TRUE)}}

  specfile     <- tempfile(pattern="mimR_")
  specfile.str <- paste(specfile,".txt", sep='')
  getfile.str  <- paste(specfile, "_out.txt", sep="")

  ##ptm0 <- proc.time()
  ##ptm1 <- proc.time()
  write(mim.cmds, specfile.str)
  ##cat("Time to write", proc.time()-ptm1, fill=T)

  ##ptm1 <- proc.time()
  system.str <- paste("mimBatch.exe ", specfile.str)
  system( system.str );
  ##cat("Time to exectute", proc.time()-ptm1, fill=T)

  ##ptm1 <- proc.time()
  look.nice.value <- scan(getfile.str , what="", sep="\n", quiet=TRUE)
  ##cat("Time to read", proc.time()-ptm1, fill=T)

  if (look.nice & length(look.nice.value)>0){
    ##ptm1 <- proc.time()
    for(i in 1:length(look.nice.value)) cat(look.nice.value[i],fill=TRUE)
    ##cat("Time to print", proc.time()-ptm1, fill=T)
  }
  unlink(specfile.str); unlink(getfile.str)

  str2 <- paste(look.nice.value, collapse=" ")
  str3 <- unlist(strsplit(str2, " "))
  value <- str3[str3!=""]
  return(invisible(value))
}


mcm <- function(){
  cat("Enter MIM commands here. Type quit to return to R\n")
  x <- readline("MIM->")
  ##cat("x", x, fill=T)
  while(length(which(c("stop","end","quit","exit","e","q")==x))==0){
    mim.out <- mim.cmd(x,look.nice=TRUE)
    x <- readline("MIM->")
    ##cat("x", x, fill=T)
  }
}

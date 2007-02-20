
.First.lib <- function(lib, pkg)
{
  if((R.version$major == 1) && (as.numeric(R.version$minor) < 9)) 
        packageDescription <- package.description 
  cat("\n-------------------------------------------------------------\n")
  cat(packageDescription("mimR", lib = lib, field="Title"))
  cat("\n")
  ver  <- packageDescription("mimR", lib = lib, field="Version")
  maint<- packageDescription("mimR", lib = lib, field="Maintainer")
  built<- packageDescription("mimR", lib = lib, field="Built")
  URL  <- packageDescription("mimR", lib = lib, field="URL")
  cat(paste("mimR, version", ver,  "is now loaded\n"))
  cat("Maintained by",maint,"\n")
  cat("Webpage:",URL,"\n")
  cat("\nBuilt:",built,"\n")
  cat("NOTICE:\n")
  cat("o mimR is available on Windows platforms only \n")
  cat("o The current version of mimR requires that MIM version 3.2.0.6 or \n")
  cat("  later (available from http://www.hypergraph.dk) is installed on the computer\n")
  cat("o IMPORTANT: While mimR automatically starts the MIM program itself if MIM is\n")
  cat("  not already running, it is RECOMMENDED that you start the MIM program \n")
  cat("  manually before starting to use mimR.\n")
  cat("\n  For more information type ?mimR\n")
  cat("-------------------------------------------------------------\n")
  return(invisible(0))
}

.Last.lib <- function(lib) {
  cat("Thank you for using mimR\n")
  return(invisible(0))
}

#  cat("  - R version 2.3.1 or later is used\n")
##  cat("  - the R package rcom is installed.\n")

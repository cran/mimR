
.First.lib <- function(lib, pkg)
##.onLoad <- function(lib, pkg)
{
  library.dynam("mimR", package = pkg, lib.loc = lib)  

   if("Rgraphviz" %in% rownames(installed.packages())){
     library(Rgraphviz)
   } else {
     cat("Note: To display models the Rgraphviz package (from Bioconductor) must be installed.\n") 
   }

  return(invisible(0))
}

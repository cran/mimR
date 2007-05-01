##
## Edges, nodes, cliques of mim objects
##

# if (!isGeneric("edges")) {
#   if (is.function("edges")) 
#     fun <- edges
#   else 
#     fun <- function(object,which) standardGeneric("edges")
#   setGeneric("edges", fun)
# }

# if (!isGeneric("nodes")) {
#   if (is.function("nodes")) 
#     fun <- nodes
#   else 
#     fun <- function(object,...) standardGeneric("nodes")
#   setGeneric("nodes", fun)
# }

setGeneric("edges", function(object,which) standardGeneric("edges"))
setMethod("edges", signature(object = "mim"),
          function(object, which) {
            x     <- object
            cl    <- cliques(x)
            value <- unlist(lapply(cl, .names2pairs),recursive=FALSE)
            value <- unique(lapply(value,sort))
            return(value)
          })

setGeneric("nodes", function(object,...) standardGeneric("nodes"))
setMethod("nodes", signature(object = "mim"),
          function(object, ...) {
            unique(unlist(cliques(object)))
          })


if (!isGeneric("cliques")) {
  if (is.function("cliques")) 
    fun <- cliques
  else 
    fun <- function(object,which) standardGeneric("cliques")
  setGeneric("cliques", fun)
}

setMethod("cliques", signature(object = "mim"),
          function(object, which) {
            x <- object
            lapply(letters2names(x$modelInfo$Cliques, .getgmData(x)),sort)
          })



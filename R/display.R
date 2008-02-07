##
## display (mimR)
## 

# if (!isGeneric("display")) {
#   if (is.function("display")) 
#     fun <- display
#   else 
#     fun <- function(x) standardGeneric("display")
#   setGeneric("display", fun)
# }

# setMethod("display", signature(x = "mim"),
#           function(x) {
#             .display.mim(x)
#           })

## .display.mim <- function(x){

plot.mim <- function(x,...){
  
  d       <- .getgmData(x)
  Delta   <- x$modelInfo$mimDelta
  Gamma   <- x$modelInfo$mimGamma
  V       <- c(Delta,Gamma)
    
  vertexColors <- rep('green', length(V))
  vertexColors[match(Gamma, V)] <- 'yellow'
  
  ord <- attr(.getgmData(x),"ordinal")
  if (!is.null(ord)){
    vertexColors[match(ord, V)] <- 'cyan'
  }
  names(vertexColors) <- V  
  
  nAttrs <- list()
  nAttrs$fillcolor <- vertexColors

  G <- new("graphNEL", nodes=V,edgemode='undirected')
  
  edges <- edges(x)
  if (length(edges)>0){
    for (i in 1:length(edges)){
      ee <- rev(edges[[i]]);  ##  cat("Adding edge:", paste(ee),"\n")
      G <- addEdge(ee[1], ee[2] , G, weight=1)
    }
  }

  plot(G, "neato", nodeAttrs = nAttrs)
  return(invisible(G))
}




#   cliques <- x$modelInfo$Cliques
#   cliques <- src2tgt (cliques, src=shortNames(d), tgt=varNames(d))

  
#   for (cl in 1:length(cliques)){
#     cliq <- cliques[[cl]]
#     if(length(cliq)>1){
#       edgeNum <- .select.order(cliq)      
#       for (i in 1:length(edgeNum)){
#         x1 <-(edgeNum[[i]]); ##print(x1)
#         G  <- addEdge(x1[1], x1[2], G, weight=1)
#       }
#     }
#   }  


# display.mim.BAK <- function(x){
  
#   d   <- .getgmData(x)
#   Delta   <- x$modelInfo$mimDelta
#   Gamma   <- x$modelInfo$mimGamma
#   V       <- c(Delta,Gamma)
    
#   vertexColors <- rep('green', length(V))
#   vertexColors[match(Gamma, V)] <- 'yellow'

#   ord <- attr(.getgmData(x),"ordinal")
#   if (!is.null(ord)){
#     vertexColors[match(ord, V)] <- 'cyan'
#   }

#   names(vertexColors) <- V  
#   nAttrs <- list()
#   nAttrs$fillcolor <- vertexColors

#   G <- new("graphNEL", nodes=V)
  
#   cliques <- x$modelInfo$Cliques
#   cliques <- src2tgt (cliques, src=shortNames(d), tgt=varNames(d))
  
#   for (cl in 1:length(cliques)){
#     cliq <- cliques[[cl]]
#     if(length(cliq)>1){
#       edgeNum <- .select.order(cliq)      
#       for (i in 1:length(edgeNum)){
#         x1 <-(edgeNum[[i]]); ##print(x1)
#         G  <- addEdge(x1[1], x1[2], G, weight=1)
#       }
#     }
#   }  
#   plot(G, "neato", nodeAttrs = nAttrs)
# }




#   if (!("package:Rgraphviz" %in% search())){
#     if("Rgraphviz" %in% installed.packages()){
#       library(Rgraphviz)
#     } else {
#       cat("The Rgraphviz package must be loaded to display the models\n")
#       return()
#     }
#   }

### END(EXPORT)

# display.mim <- function(x){

#   if (!("package:Rgraphviz" %in% search())){
#     if("Rgraphviz" %in% installed.packages()){
#       library(Rgraphviz)
#     } else {
#       cat("The Rgraphviz package must be loaded to display the models\n")
#       return()
#     }
#   }
  
#   x <- x
#   names   <- x$data$name
#   fact    <- x$data$factor
#   Gamma   <- x$modelInfo$mimGamma
#   V       <- as.character(names)
  
#   vertexColors <- rep('green', length(V))
#   vertexColors[match(Gamma, V)] <- 'yellow'
#   names(vertexColors) <- V
  
#   nAttrs <- list()
#   nAttrs$fillcolor <- vertexColors

#   G <- new("graphNEL", nodes=V)
  
#   cliques <- x$modelInfo$Cliques
#   cliques <- src2tgt (cliques, src=x$data$letter, tgt=x$data$name)   
#   for (cl in 1:length(cliques)){
#     cliq <- cliques[[cl]]
#     if(length(cliq)>1){
#       edgeNum <- .select.order(cliq)
      
#       for (i in 1:length(edgeNum)){
#         x1<-(edgeNum[[i]]); ##print(x1)
#         G <- addEdge(x1[1], x1[2], G, weight=1)
#       }
#     }
#   }
  
#   plot(G, "neato", nodeAttrs = nAttrs)
# }

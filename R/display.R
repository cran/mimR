### BEGIN(EXPORT)

display <- function(x) UseMethod("display")
display.mim <- function(x){

  if (!("package:Rgraphviz" %in% search())){
    if("Rgraphviz" %in% installed.packages()){
      library(Rgraphviz)
    } else {
      cat("The Rgraphviz package must be loaded to display the models\n")
      return()
    }
  }
  
  m12 <- x
  names   <- m12$data$name
  fact    <- m12$data$factor
  Gamma   <- m12$modelInfo$mimGamma
  V       <- as.character(names)
  
  vertexColors <- rep('green', length(V))
  vertexColors[match(Gamma, V)] <- 'yellow'
  names(vertexColors) <- V
  
  nAttrs <- list()
  nAttrs$fillcolor <- vertexColors

  G <- new("graphNEL", nodes=V)
  
  cliques <- m12$modelInfo$Cliques
  cliques <- src2tgt (cliques, src=m12$data$letter, tgt=m12$data$name)   
  for (cl in 1:length(cliques)){
    cliq <- cliques[[cl]]
    if(length(cliq)>1){
      edgeNum <- .select.order(cliq)
      
      for (i in 1:length(edgeNum)){
        x1<-(edgeNum[[i]]); ##print(x1)
        G <- addEdge(x1[1], x1[2], G, weight=1)
      }
    }
  }
  
  plot(G, "neato", nodeAttrs = nAttrs)
}

### END(EXPORT)

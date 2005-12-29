display <- function(x) UseMethod("display")
display.mim <- function(x){
  m12 <- x
  names   <- m12$data$name
  fact    <- m12$data$factor
  
  V <- as.character(names)
  letter   <- as.character(m12$data$letter)
  EdLi <- vector("list", length=length(V))
  names(EdLi) <- V
  
  cliques <- m12$modelInfo$Cliques
  
  for (cl in 1:length(cliques)){
    cliq <- cliques[[cl]]
    
    cliqNum <- match(cliq, letter)
    edgeNum <- lapply(.select.order (cliqNum),as.numeric)
    
    for (i in 1:length(edgeNum)){
      x1<-(edgeNum[[i]][1])
      x2<-(edgeNum[[i]][2])
      EdLi[[x1]]$edges <- c(EdLi[[x1]]$edges, x2)
    }
  }
  
  G <- new("graphNEL", nodes=V, edgeL=EdLi)
  nAttrs <- list()
  nAttrs$fillcolor <- rep("pink", length(which(fact)))
  names(nAttrs$fillcolor) <- V[which(fact)]
  
  plot(G, "neato", nodeAttrs=nAttrs)
}

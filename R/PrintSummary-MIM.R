
print.mim <- function(x, ...){
  #print.default(x)
  #cat("------------------------------------\n")
  cat("Formula:", .mimFormula(x),"\n")
  cat("Formula(letter):", .mimFormula.letter(x),"\n")
  if (.is.fitted(x)){
    cat("likelihood:", .likelihood(x), "DF:", .DF(x),"\n")
  }
  if (!is.null(.latent.in.model(x)))
    cat("Latent variables in model:", .latent.in.model(x),"\n")
  #cat("My checking:\n")
  #print(names(x))
  #print(x$used.names)
}

summary.mim <- function(object, ...){
  cat("Formula:",.mimFormula(object),"\n")
  cat("Formula(letter):", .mimFormula.letter(object),"\n")
  if (.is.fitted(object)){
    cat("deviance:", as.numeric(deviance(object)),  "DF:", .DF(object), "likelihood:", .likelihood(object),"\n")
  }
  cat("Model properties:\n")
  if (!is.null(.latent.in.model(object)))
    cat(" Latent variables in model:", paste(.latent.in.model(object),collapse=' '),"\n")
  cat(" Variables in model:       ", object$used.names,"\n")
  cat(" Is graphical:             ", object$modelInfo$Graphical,"\n")
  cat(" Is decomposable:          ", object$modelInfo$Decomposable,"\n")
  cat(" Is mean linear:           ", object$modelInfo$Mean.Linear,"\n")
  cat(" Is homogeneous:           ", object$modelInfo$Homogeneous,"\n")
  cat(" Is delta-collapsible:     ", object$modelInfo$Delta.Collapsible,"\n")
  cat(" Degrees of freedom:       ", .DF(object), "\n")
  
  cat(" Cliques:\n")
  cl <-lapply(.cliques(object),.look.up.mim.names, object$data, "from.mim")
  print(unlist(lapply(cl, paste, collapse=':')))
}

print.mim <- function(x, ...){
  cat("Formula:", .mimFormula(x),"\n")
  if (.is.fitted(x)){
    cat("likelihood:", .likelihood(x), "DF:", .DF(x),"\n")
  }
  if (!is.null(.latent.in.model(x)))
    cat("Latent variables in model:", .latent.in.model(x),"\n")
}

summary.mim <- function(object, ...){
  cat("Formula:",.mimFormula(object),"\n")
  cat("Formula(letter):", .mimFormula.letter(object),"\n")
  cat("Variable type:", variableType(object),"\n")
  if (.is.fitted(object)){
    cat("deviance:", as.numeric(deviance(object)),  "DF:", .DF(object), "likelihood:",
        .likelihood(object),"\n")
  }
  if (!is.null(.latent.in.model(object))){
    cat(" Latent variables in model:", paste(.latent.in.model(object),collapse=' '),"\n")
    cat(" Note: The degrees of freedom reported above may not be correct\n")
  }
  
  cat(" Cliques: ")
  cl <-lapply(.cliques(object),.lettersToNames, object$data)
  print(unlist(lapply(cl, paste, collapse=':')))
  cat("For model properties, use        : 'properties()'\n")
  cat("For fitting information etc. use : 'modelInfo()'\n")
}

properties                    <- function(object) UseMethod("properties")
properties.mim <- function(object){
  cat(" Variables in model  : ", object$used.names,"\n")
  cat(" Is fitted           : ", object$modelInfo$Fitted, "\n")
  cat(" Is graphical        : ", object$modelInfo$Graphical)
  cat("   Is decomposable:", object$modelInfo$Decomposable,"\n")
  cat(" Is mean linear      : ", object$modelInfo$Mean.Linear)
  cat("   Is homogeneous :", object$modelInfo$Homogeneous,"\n")
  cat(" Is delta-collapsible: ", object$modelInfo$Delta.Collapsible,"\n")
}


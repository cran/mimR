
print.gmData        <- function(x, ...){
  print.data.frame(x);

  if (!is.null(attr(x,'ordinal'))){
    cat("Ordinal     :     ", attr(x,'ordinal'),"\n")
  }
  
  if (!is.null(.dataOrigin(x)))
    cat("Data origin :     ", .dataOrigin(x),"\n")
  else
    cat("Data origin :     ", "no data", "\n")
  if (!is.null(latent(x)))
    cat("Latent variables:", paste(latent(x),collapse=' '), "\n")
  return(x)
}

print.mim <- function(x, ...){
  cat("Formula:", .mimFormula(x),"\n")
  if (!is.null(.latentInModel(x)))
    cat("Latent variables in model:", .latentInModel(x),"\n")
  if (.is.fitted(x)){
    cat("-2logL:", .likelihood(x), "DF:", .DF(x), "\n")
  }
}

summary.mim <- function(object, ...){
  value <- list(
                Formula   = mimFormulaNames(object),
                Variables = object$used.names,
                isfitted  = .is.fitted(object),
                deviance  = deviance(object),
                DF        = .DF(object),
                likelihood = .likelihood(object),
                latent     = .latentInModel(object)
              )
  class(value) <- 'mimsummary' 
  return(value)
}


properties                    <- function(object) UseMethod("properties")
properties.mim <- function(object){
  value <- list(
                Variables = object$used.names,
                Cliques   = lapply(.cliques(object),letters2names, object$data),
                graphical        = modelInfo(object, "Graphical"),
                decomposable     = modelInfo(object, "Decomposable"),
                meanlinear       = modelInfo(object, "Mean.Linear"),
                homogeneous      = modelInfo(object, "Homogeneous"),
                deltacollapsible = modelInfo(object, "Delta.Collapsible")
                )
  class(value) <- 'mimproperties'
  return(value)
}


print.mimproperties <- function(x, ...){
  cat("Model properties:\n")
  cat(" Variables in model  : ", x$Variables,"\n")
   cat(" Cliques: ")
  print(unlist(lapply(x$Cliques, paste, collapse=':')))
  cat(" Is graphical        : ", x$graphical)
  cat("   Is decomposable:", x$decomposable, "\n")
  cat(" Is mean linear      : ", x$meanlinear)
  cat("   Is homogeneous :", x$homogeneous)
  cat("   Is delta-collapsible: ", x$deltacollapsible,"\n")
}




print.mimsummary <- function(x, ...){
  cat("Formula:",x$Formula,"\n")
  cat("Variables in model  : ", x$Variables,"\n")  
  
  if (x$isfitted){
    cat("deviance:", x$deviance,  "DF:", x$DF, "likelihood:",
        x$likelihood,"\n")
  }
  if (!is.null(x$latent)){
    cat(" Latent variables in model:", paste(x$latent,collapse=' '),"\n")
    cat(" Note: The degrees of freedom reported above may not be correct\n")
  }
}





# properties.mim <- function(object){
#   cat("Model properties:\n")
#   cat(" Variables in model  : ", object$used.names,"\n")
#    cat(" Cliques: ")
#   cl <-lapply(.cliques(object),letters2names, object$data)
#   print(unlist(lapply(cl, paste, collapse=':')))
#   ##  cat(" Is fitted           : ", object$modelInfo$Fitted, "\n")
#   cat(" Is graphical        : ", object$modelInfo$Graphical)
#   cat("   Is decomposable:", object$modelInfo$Decomposable,"\n")
#   cat(" Is mean linear      : ", object$modelInfo$Mean.Linear)
#   cat("   Is homogeneous :", object$modelInfo$Homogeneous)
#   cat("   Is delta-collapsible: ", object$modelInfo$Delta.Collapsible,"\n")
# }







  
print.modelInfo <- function(x,...){
  nam<- names(x)
  len <- lapply(x,length)

  for (i in 1:length(nam)){
    if (len[i]==1){
      if (is.list(x[[i]])){
        cat("slot:", nam[i],"\n")
        print(x[[i]])
      } else
      cat("slot:", nam[i], ":", x[[i]],"\n")
    } else {
      cat("slot:", nam[i],"\n")
      print(x[[i]])
    }
  }
}

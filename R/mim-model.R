

mim <- function(mimFormula, data, fit=TRUE, marginal=data$name){

  mimFormula2Formula.as.string <- function(mimFormula, data){
    l <- string2listNames(mimFormula)
    a <- src2tgt(l, src=varNames(data), tgt=shortNames(data))
    Formula.as.string <- list2stringLetters(a)
    return(Formula.as.string)
  }
  
  data <- switch(class(data)[1],
                 'data.frame'=,'table'={as.gmData(data)},
                 'gmData'={data},
                 {stop("Data must be either a dataframe, a table or a gmData.")})
  
  mimFormula <- .handleSpecialModels(mimFormula, data, marginal=marginal)

  Formula.as.string <- mimFormula2Formula.as.string(mimFormula,data)
  .varspec.toMIM(data)
  
  str  <- paste("Model ", Formula.as.string); mim.cmd(str)
  rsm  <- .RSmodel();     

  value <-list("mimFormula"        =mimFormula,
               ##"mimFormula.letter" =Formula.as.string,
               "modelInfo"         =rsm,
               "data"              =data,
               "fit"               =fit)
  value$used.names <- .namesInModel(value)
  class(value) <- "mim"
  
  if (is.null(obs(data)) & fit!=FALSE){
    cat("gmData contains no data. Can not fit model\n")
    return(value)
  }  

  if (fit==TRUE) 
    value <- fit(value)
  if (is.character(fit))
    value <- fit(value, fit)
  return(value) 
}

.handleSpecialModels <- function(mimFormula, data, marginal){
  model.type <-switch(mimFormula,
                      ".."  = {"SatMod"   },
                      "..h" = {"HomSatMod"},
                      "."   = {"Main"     })
  if (!is.null(model.type)){
    .varspec.toMIM(data)
    marg <- names2letters(marginal, data)
    str  <- paste(model.type, paste(marg,collapse=' ')); mim.cmd(str)
    rsm  <- .RSmodel();    
    mimFormula <- rsm$mimFormula.as.string
  } 
  return(mimFormula)
}



mim <- function(mimFormula, data, fit=TRUE, marginal=data$name){

  mimFormula2Formula.as.string <- function(mimFormula, data){
    l <- string2listNames(mimFormula)
    a <- src2tgt(l, src=varNames(data), tgt=shortNames(data))
    Formula.as.string <- list2stringLetters(a)
    return(Formula.as.string)
  }
  
  data <- switch(class(data)[1],
                 'data.frame'=,'table'=,'xtabs'={as.gmData(data)},
                 'gmData'={data},
                 {stop("Data must be either a dataframe, a table or a gmData.")})
  
  mimFormula <- .handleSpecialModels(mimFormula, data, marginal=marginal)

  #print(mimFormula)
  
  Formula.as.string <- mimFormula2Formula.as.string(mimFormula,data)
  .varspec.toMIM(data)

  str  <- paste("Model ", Formula.as.string);
                                        #print(str)
  str2 <- .str2strlist(str)
  lapply(str2, mim.cmd)
  
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
    #cat("marg:", marg, "\n")
    str  <- paste(model.type, paste(marg,collapse=' '));
    #cat("str:", str, "\n")
    mim.cmd(str)
    rsm  <- .RSmodel();
    #print(rsm)
    mimFormula <- rsm$mimFormula.as.string
  } 
  return(mimFormula)
}

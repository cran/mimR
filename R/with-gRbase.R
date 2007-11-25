.findVarspec <- function(nt){

  factb <- varTypes(nt) %in% c("Discrete", "Ordinal")

  
  var.spec <-
    paste(paste("Fact", paste(shortNames(nt)[factb], nLevels(nt)[factb], collapse=' '),";"),
          paste("Cont", paste(shortNames(nt)[!factb], collapse=' ')))
  
  lab.spec <-
    paste("Labels", shortNames(nt),
          gsub(' ','',paste('\"',varNames(nt),'\"'))     )


  ordn <- varNames(nt)[varTypes(nt)=="Ordinal"]
  ords <- shortNames(nt)[varTypes(nt)=="Ordinal"]

  ord.spec <-
    c(paste("# Ordinal", paste(ordn, collapse=" ")),
      paste("Ordinal", paste(ords, collapse=" ")))

  
  vl    <- valueLabels(nt)
  vllen <- length(vl)
  vallab.spec <- as.list(rep(NA,vllen))
  fshort <- shortNames(nt)[match(names(vl), varNames(nt))]
  
  if (vllen){
    for (i in 1:vllen){     
      vc    <- vl[[i]]
      v     <- fshort[i]
      vlev  <- 1:nLevels(nt)[match(v, shortNames(nt))]
      x<-paste(paste("ValLabel", v), paste(vlev, gsub(' ','',paste('\"',vc,'\"')), collapse=' '))
      vallab.spec[[i]]  <- x
    }
  }
  
  vallab.spec <- unlist(vallab.spec)
  value<-list("var.spec"=var.spec, "lab.spec"=lab.spec,"vallab.spec"=vallab.spec,
              "ord.spec"=ord.spec)
  return(value)
}























# varTypes.mimgmData <- function (x) 
# {
#     x$varTypes
# }

# varNames.mimgmData <- function (x) 
#   as.vector(x$varNames)

# nLevels.mimgmData <- function (x) 
#   as.vector(x$nLevels)

# latent.mimgmData <- function (x) 
# {
#     attr(x, "latent")
# }

# valueLabels.mimgmData <- function (x) 
#   attr(x, "valueLabels")



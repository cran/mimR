##################################################################################
makeNameTable <- function(data) UseMethod("makeNameTable")
##################################################################################

makeNameTable.data.frame <- function(data){
  name      <- names(data)
  vallabels <- lapply(1:ncol(data), function(j) levels(data[,j]))
  names(vallabels) <- name
  factor    <- unlist(lapply(vallabels,length))
  vallabels <- vallabels[!unlist(lapply(vallabels, is.null))]
  ##print(vallabels)
  value     <- makeNameTablePrimitive(name, factor=factor, vallabels=vallabels)
  return(value)
}

makeNameTable.table <- function(data){
  vallabels <- dimnames(data)
  name      <- names(vallabels)
  factor    <- unlist(lapply(vallabels,length))
  
  value <- makeNameTablePrimitive(name, factor=factor, vallabels=vallabels)
  return(value)
}

makeNameTablePrimitive <- function(name, letter=NULL,
                factor,#=rep(FALSE,length(name)),
                vallabels=NULL, data=NULL){
  nam <- name
  len     <- length(nam)

  letter <- c(letters,LETTERS)[1:length(name)]

  if (length(vallabels)==0)
    vallabels <- NULL
  if (!is.null(vallabels)){
    ##cat("Doing from vallabels\n")
    ##print(vallabels); print(len)
    vlname      <- names(vallabels)
    vllev       <- sapply(vallabels, length)
    fvec        <- rep(FALSE, len); names(fvec) <- nam
    lvec        <- rep(NA, len);    names(lvec) <- nam
    fvec[vlname] <- TRUE
    lvec[vlname] <- vllev
  } else {
    if (missing(factor)){
      ##cat("No factor given - assuming everything to be binary factors\n")
      fvec      <- rep(TRUE,len)
      lvec      <- rep(2, len);  names(lvec) <- nam 
      vallabels <- mapply(function(v,l){paste(v,1:l,sep='.')}, nam, lvec,SIMPLIFY=FALSE)
    } else {
      fvec <- factor
      if (is.logical(factor)){
        ##cat("Factor - logical\n")
        isf <- which(as.logical(fvec))
        lvec      <- rep(NA, len);  names(lvec) <- nam
        lvec[isf] <- 2
      } else {
        ##cat("Factor - numerical\n")
        isf <- which(as.logical(fvec))
        len     <- length(nam)
        lvec    <- rep(NA, len);    names(lvec) <- nam
        lvec[nam[isf]] <- fvec[isf]
        fvec    <- as.logical(fvec)
      }
      vallabels <- mapply(function(v,l){paste(v,1:l,sep='.')},
                          nam[isf], lvec[isf],SIMPLIFY=FALSE)
    }   
  }
  value <- data.frame(name   =nam,    letter =letter,
                      factor =fvec,   levels =lvec)
  value$name      <- as.character(name)
  value$letter    <- as.character(letter)

  rownames(value) <- 1:nrow(value)
  attr(value, "vallabels")<- vallabels
  return(value)
}


# .makeNameTablePrimitive.BAK <- function(name, letter=NULL,
#                                    factor=rep(FALSE,length(name)),
#                                    vallabels=NULL, data=NULL){
#   if (is.null(letter))
#     letter <- c(letters,LETTERS)[1:length(name)]

#   levels            <- factor
#   levels[levels==0] <- NA
#   factor            <- factor!=0
  
#   vallabels2 <-
#     mapply(function(n,l)paste(n, 1:l,sep='.'),
#            name[factor], levels[factor], SIMPLIFY=FALSE)

#   if (length(vallabels2)>0){
#     if (length(vallabels)>0 ){
#       for (i in 1:length(names(vallabels))){
#         ii<-match(names(vallabels)[i], names(vallabels2))
#         if (length(ii)>0 && !is.na(ii))
#           vallabels2[ii] <- vallabels[ii] 
#       }
#     }
#   }
  
#   value           <- data.frame(name,letter,factor,levels)
#   value$name      <- as.character(name)
#   value$letter    <- as.character(letter)
#   rownames(value) <- 1:nrow(value)  
#   attr(value,"vallabels")      <- vallabels2
#   return(value)
# }

# .makeNameTablePrimitive <- function(name, letter=NULL,
#                                    factor,#=rep(FALSE,length(name)),
#                                    vallabels=NULL, data=NULL){
#   cat("name      :\n"); print(name);
#   ##cat("letter    :\n"); print(letter)
#   cat("factor    :\n"); if (!missing(factor)) print(factor)
#   cat("vallabels :\n"); print(vallabels)
#   if (is.null(letter))
#     letter <- c(letters,LETTERS)[1:length(name)]

#   len       <- length(name)
#   factorvec <- rep(NA, len)
#   vlname    <- names(vallabels)
#   vllev     <- lapply(vallabels, length)
#   levels    <- rep(NA, len)
  
#   levels[match(vlnam, nam)] <- vllev
#   print(levels)
  
# #   if (missing(factor)){ # Default: All variables are factors
# #     factor<- rep(FALSE,length(name))
# #     levels<- rep(2,length(name))
# #   } else {
# #     levels <- rep(NA, length(name))
# #     levels[factor] <- 2
# #     #print(factor)
# #     #print(vallabels)
# #     #print(levels)
    

#                                         #levels            <- factor
#     #levels[levels==0] <- NA
#     #factor            <- factor!=0
#   #}
#   vallabels2 <-
#     mapply(function(n,l)paste(n, 1:l,sep='.'),
#            name[factor], levels[factor], SIMPLIFY=FALSE)

#   if (length(vallabels2)>0){
#     if (length(vallabels)>0 ){
#       for (i in 1:length(names(vallabels))){
#         ii<-match(names(vallabels)[i], names(vallabels2))
#         if (length(ii)>0 && !is.na(ii))
#           vallabels2[ii] <- vallabels[ii] 
#       }
#     }
#   }
  
#   value           <- data.frame(name,letter,factor,levels)
#   value$name      <- as.character(name)
#   value$letter    <- as.character(letter)
#   rownames(value) <- 1:nrow(value)  
#   attr(value,"vallabels") <- vallabels2
#   return(value)
# }




testdelete <- function(edge,mim,options=NULL){
  d <- .getgmData(mim)
  e <- names2letters(edge,d)
  .RStestdelete(e,options)
}

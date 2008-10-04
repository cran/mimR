
names2letters <- function(x, data){
  src2tgt(x, src=varNames(data), tgt=shortNames(data))
}
letters2names <- function(x, data){
  src2tgt(x, src=shortNames(data), tgt=varNames(data))
}


mimFormulaLetters <- function(object)  object$modelInfo$Formula.as.string
mimFormulaNames   <- function(object)  object$mimFormula

src2tgt           <- function(x, src, tgt){  UseMethod("src2tgt")}
src2tgt.character <- function(x, src, tgt){  tgt[match(x, src)]}
src2tgt.list      <- function(x, src, tgt){  lapply(x, src2tgt, src, tgt)}
src2tgt.NULL      <- function(x, src, tgt){  NULL }

string2listNames <- function(mimFormula){
  s1 <- unlist(strsplit(mimFormula, "/"))
  if (length(s1)==2)    s1 <- s1[1]
  s1 <- c(s1, NA, NA)[1:3]

  s3 <-lapply(s1, function(s){
    if(is.na(s))
      list()
    else {
      s2 <- strsplit(s, "\\+")    
      s2 <- unlist(lapply(s2, function(x)gsub(" +","",x)))
      s3 <- strsplit(s2,":")
      s3
    }
  })
  names(s3) <- c("discrete", "linear", "quadratic")
  return(s3)
}

string2listLetters <- function(mimFormula){
  s1 <- unlist(strsplit(mimFormula, "/"))
  if (length(s1)==2)    s1 <- s1[1]
  s1 <- c(s1, NA, NA)[1:3]
  
  s3 <-lapply(s1, function(s){
    if(is.na(s))
      list()
    else {
      s2 <- strsplit(s, "[+,]")    
      s2 <- unlist(lapply(s2, function(x)gsub(" +","",x)))
      s3 <- strsplit(s2,"")
      s3
    }
  })
  names(s3) <- c("discrete", "linear", "quadratic")
  return(s3)
}

list2stringNames <- function(a){
  l2 <- lapply(a, lapply, paste, collapse=':')
  l3 <- lapply(l2, paste, collapse=' + ')
  l4 <- paste(l3,collapse='/')
  return(l4)
}

list2stringLetters <- function(a){
  l2 <- lapply(a, lapply, paste, collapse='')
  l3 <- lapply(l2, paste, collapse=',')
  l4 <- paste(l3,collapse='/')
  return(l4)
}



.names2pairs <- function(x){
  idx <- 1:length(x)
  val <- NULL
  for (i in 1:length(idx)){
    val <- c(val, lapply(idx[-(1:i)],c,i))
  }
  val <- lapply(val,sort)
  value <- lapply(val,function(y){x[y]})
  return(value)
}


## Use this for dealing with long input strings to MIM
##
.str2strlist <- function(x, len=50, pad="&"){
  ans <- list()
  ii <- 0
  repeat{
    if (nchar(x)==0)
      break
    ii <- ii + 1
    ans[[ii]] <- substr(x, 1, len)
    x <- substr(x,len+1, 100000)
  }
  if (nchar(pad)>0 && ii>1){
    for (jj in 1:(ii-1)){
      ans[[jj]] <- paste(ans[[jj]], pad, collapse=" ")
    }
  }
  ans
}

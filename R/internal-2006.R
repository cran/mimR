
names2letters <- function(x, data){
  src2tgt(x, src=data$name, tgt=data$letter)
}
letters2names <- function(x, data){
  src2tgt(x, src=data$letter, tgt=data$name)
}


mimFormulaLetters <- function(mim)  mim$modelInfo$Formula.as.string
mimFormulaNames   <- function(mim)  mim$mimFormula

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

###  Used in gRbase ###
.all.subsets <- function(x){
  if (length(x)==1)
    return(x)
  else {
    val <- x[1]
    for (i in 2:length(x)){
      v <- paste(val,x[i],sep='+')
      val <- c(val,x[i],v)
    }
    val <- strsplit(val,"\\+")
    return(val)
  }
}

.select.order  <- function(x,order=2){
  v <- .all.subsets(x)
  value <- v[lapply(v,length)==as.numeric(order)]
  return(value)
}


nthOrderModel <- function(variables,order=2){
  v <-.select.order(variables,order)
  v <- lapply(v, paste, collapse=":")
  v <- paste(unlist(v),collapse="+")
  value <- v
  #value <-as.formula(paste("~",v))
  return(value)
}

###  Used in gRbase - END ###



.find.edges <- function(set=NULL){
  if (is.null(set)){
    mo <- mim.print("m",verbose=TRUE)$mim.discrete
    mo <- unlist(strsplit(mo,"\\."))
  }
  else
    mo <- set
  v <- select.order(mo,order=2)
  v <- as.vector(unique(unlist(v)))
  return(invisible(v))  
}

.max.order.model <- function(order){
  mo <- mim.print("m",verbose=TRUE)$discrete;   print(mo)
  mo <- unlist(strsplit(mo,"\\."))
  v  <- select.order(mo,order=order)
  v  <- unique(unlist(v))
  model.str <- paste(v,collapse=',')
  mim.cmd(paste("model ", model.str))
  mim.print("m");
  v <- mim.output.to.list(mim.cmd("fit"))
}


.mmm <- function(set, order=nchar(set)){
  if (order <= 0)
    real.order <- nchar(set)+order
  else
    real.order <- order
  v1  <- as.vector(unlist(select.order(set,order=real.order)))
  v2  <- as.vector(unlist(select.order(set,order=real.order-1)))
  ##print(c(v1, v2))
  
  base.formula <- paste(c(v1,v2), collapse=",")
  print(base.formula)

  mim.cmd(paste("model ", base.formula, "; fit; base"))
  curr.list <- lapply(v1, function(x){
    vm <- v1[x!=v1]
    formula.str <- paste(c(vm, v2),collapse=",")
    cat("Dropping term :", x, fill=TRUE)
    mim.cmd(paste("model ", formula.str, "; fit;"))
    aa <- mim.cmd("test",look.nice=TRUE)
    
  })
}








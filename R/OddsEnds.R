doodle.mim <- function(x){
  cat("No doodle (graphical device) available for the time being\n")
  cat("However, it is possible to interact with a graph as follows:\n")
  cat("1) for a mim model object m with data d, first do 'fit(m)'\n")
  cat("2) then go to the MIM program and edit the model\n")
  cat("3) then do m2 <- retrieve(d) \n")
}
doodle <- function(x) UseMethod("doodle",x)




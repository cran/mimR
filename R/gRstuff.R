


.nt.to.varspec <- function(nt){
  var.spec <-
    paste(
          paste("Fact", paste(nt$letter[nt$factor==TRUE],nt$levels[nt$factor==TRUE],collapse=' ')), ";",
          paste("Cont", paste(nt$letter[nt$factor==FALSE],collapse=' '))  )
  
    lab.spec <- paste("Labels", nt$letter,
                         gsub(' ','',paste('\"',nt$name,'\"'))
                         )
  value<-list("var.spec"=var.spec, "lab.spec"=lab.spec)
}


  









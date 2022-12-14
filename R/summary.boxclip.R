summary.boxclip<-function(object,...){

  cat("clipit :",object$clipit, "\n")
  cat("width  :",object$width,"\n")
  cat("height :",object$height,"\n")
  cat("\n")
  cat("Full w/o NA: ",object$full,"\n")
  cat("Remaining  : ",object$rest,"\n")
  rest<-round(abs(object$rest/object$full*100),1)
  cat (rest,"% values remain \n")

}

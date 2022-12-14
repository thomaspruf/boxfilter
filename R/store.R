store<-function(object){
  name=readline(prompt="Enter directory/file name: ")
  df=data.frame(x=object$x,y=object$y,filtered=object$filtered)
  if (name!="") {
    write.csv(df,name,row.names=FALSE)
    print(paste("storing",name))
  }
}

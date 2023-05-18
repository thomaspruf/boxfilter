showdata=function (x,y){
  nn=length(y)

  change=abs(diff(y))

  df=data.frame(x=x,y=y)

  p1 <- ggplot(data=df, aes(x,y)) +geom_point(color="blue",show.legend=FALSE,size=1)+ geom_line()
  p1=p1+theme_gray(18)
  df=data.frame(change=change)

  p3=ggplot(data=df,mapping=aes(change))
  p3=p3+geom_histogram(closed="left",bins=10,color="blue",fill="gray80")
  p3=p3+theme_gray(18)

  grid.arrange(p1,p3,ncol=1)


  x=as.POSIXct(x,tz="GMT", origin = "1970-01-01")
  xt=0
  for (i in 2:12){
  xt[i]=difftime(x[i],x[i-1],units="hours")
  }

  xt=xt[-1]

  mt=mean(xt,na.rm=TRUE)
  unit="hours."
  if (mt==0){
     mt=1
     unit="points."

  }
  mt=round(mt,2)
  writeLines          ( "")
  writeLines          ( "")
  writeLines          ( "---------------------------------------------------------------------------------------------------")
  writeLines(paste("Average sampling interval",mt,unit))
  r=readline(prompt="Boxfilter may eliminate rare excursions to genuine values that have few neighbors. Continue (y/n) ? ")

  writeLines          ( "---------------------------------------------------------------------------------------------------")
  if (tolower(r)=="n") stop()
  }






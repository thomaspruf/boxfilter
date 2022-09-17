clipview<-function(x,y, width=NULL, height= NULL, clipit=NULL, miny=10){

  filtered<-neighbors<-NULL
  if (is.null(x)) x<-1:length(y)
  if (is.null(width))  width<-floor(length(x)*0.01)
  if (is.null(height)) height<-floor( mean(y,na.rm=T)/4)

  ndat<-boxclip(x=x,y=y, width=width, height= height, clipit=NULL,plotit=FALSE)
  dat<-data.frame(x=ndat$x,y=ndat$y,neighbors=ndat$neighbors)

  if (is.null(clipit)) clipit<-seq(0.1,0.5,by=0.1)

  # original data
  p1 <- ggplot(dat, aes(x,y))+geom_point(color="darkblue",size=0.8)
  p1<-p1+ylab("original")
  p1<-p1+theme_gray(18)
  #histogram
  p2<-  ggplot( dat, aes(neighbors))+geom_histogram(bins=10,color="blue",fill="gray80")
  p2<-p2+xlab("neighbors")
  p2<-p2 + scale_y_log10()
  p2<-p2+theme_gray(18)

  #Four filtered data sets
  col<-"red"
  dat$filtered<-NA
  ix<-which (ndat$y<miny)
  dat$neighbors[ix]<-0

  ix<-which(ndat$neighbors>clipit[1])
  dat$filtered[ix]<-ndat$y[ix]
  p3<-ggplot(dat, aes(x,filtered))+geom_point(color=col,size=0.8)
  p3<-p3+theme_gray(18)
  p3<-p3+ggtitle(clipit[1])

  dat$filtered<-NA
  ix<-which(ndat$neighbors>clipit[2])
  dat$filtered[ix]<-ndat$y[ix]
  p4<-ggplot(dat, aes(x,filtered))+geom_point(color=col,size=0.8)
  p4<-p4+theme_gray(18)
  p4<-p4+ggtitle(clipit[2])

  dat$filtered<-NA
  ix<-which(ndat$neighbors>clipit[3])
  dat$filtered[ix]<-ndat$y[ix]
  p5<-ggplot(dat, aes(x,filtered))+geom_point(color=col,size=0.8)
  p5<-p5+theme_gray(18)
  p5<-p5+ggtitle(clipit[3])

  dat$filtered<-NA
  ix<-which(ndat$neighbors>clipit[4])
  dat$filtered[ix]<-ndat$y[ix]
  p6<-ggplot(dat, aes(x,filtered))+geom_point(color=col,size=0.8)
  p6<-p6+theme_gray(18)
  p6<-p6+ggtitle(clipit[4])

  grid.arrange(p1,p2,p3,p4,p5,p6,ncol=2,nrow=3)
}

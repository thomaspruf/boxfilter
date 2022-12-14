
boxclip <- function(x,y,clipit=NULL,QI=NULL,width=NULL, height=NULL, miny=10, plotit=TRUE,histo=FALSE)  {

  # small values of clipit (0-1) are more tolerant

  filtered<-neighbors<-NULL

  if (is.null(x)) x<-1:length(y)
  if (is.null(width))  width<-floor(length(x)*0.01)
  if (is.null(height)) height<-floor( mean(y,na.rm=T)/4)

  dat=data.frame(x,y)
  n=dim(dat)[1]
  empty=rep(NA,(n+2*width))
  ndat=data.frame(x=empty,y=empty)

  ndat[1:width,]<-dat[1:width,] #left
  ndat [(width+1):(width+n),] <-dat #center
  ndat [(width+n+1):(2*width+n),]<-dat[1:width,] #right

  ndat$neighbors<-0
  nn<-dim(ndat)[1]

  for (i in (width+1):(nn-width) )  {
      this <- ndat$y[i]
      if(!is.na(this)){
         sect <- (i-width):(i+width)
         hi<-this+height
         lo<-this-height
         test<-ndat$y[sect]
         nei<-sum(test>=lo & test<=hi,na.rm=T)
         all=length(ndat$y[sect])
         ndat$neighbors[i]<-nei/all
      }
  }

  ndat<-ndat[(width+1):(width+n),]
  class(ndat$x)<-class(dat$x)

  ix<-which (ndat$y<miny)
  ndat$neighbors[ix]<-0


  if (is.null(clipit)){
    # make clipit from 1st trough in hist
     if(length(y)<10000) bb=0.1 else bb=0.05
     h<-hist(ndat$neighbors,plot=FALSE,breaks=seq(0,1,by=bb),right=F)
     i<-1:length(h$counts)
     mxi<-which.max(h$density[i>1])
     part<-h$counts[2:mxi]
     which.mm<-which.min(part)+1
     clipit<-h$mids[which.mm]
     clipit<-round(clipit,3)
  }


  ndat$filtered<-NA
  ix<-which(ndat$neighbors>clipit)
  ndat$filtered[ix]<-ndat$y[ix]

  if (is.null(QI)) ndat$QI<-0

  if (plotit==TRUE){
   if (is.null(QI)){
     p1 <- ggplot(ndat, aes(x,y))+geom_point(aes(color=QI),show.legend=FALSE)
     p1<-p1+theme_gray(20)
     p2<-  ggplot(ndat, aes(x,filtered))+geom_point(color="red")
     p2<-p2+theme_gray(20)

   } else {
     p1 <- ggplot(ndat, aes(x,y))+geom_point(aes(color=QI))
     p1<-p1+theme_gray(20)
     p2<-  ggplot(ndat, aes(x,filtered))+geom_point(color="red")
     p2<-p2+theme_gray(20)
     p2<-p2+theme(plot.margin=margin(t = 0, r = 85, b = 0, l = 0, unit = "pt"))
   }

    if (histo==FALSE){
        grid.arrange(p1,p2,ncol=1)

    } else {
        p3<-  ggplot( ndat, aes(neighbors))+geom_histogram(closed="left",bins=10,color="blue",fill="gray80")
        p3<-p3+xlab("neighbors")
        p3<-p3 + scale_y_log10()
        p3<-p3+theme_gray(18)
        if (!is.null(QI)) p3<-p3+theme(plot.margin=margin(t = 0, r = 85, b = 0, l = 0, unit = "pt"))
        grid.arrange(p1,p2,p3,ncol=1)
    }
  }

  full<-length(na.omit(ndat$y))
  rest=length(na.omit(ndat$filtered))

  clipout=(list(x=x,y=y,filtered=ndat$filtered, neighbors=ndat$neighbors,clipit=clipit, width=width,height=height,full=full, rest=rest))

  class(clipout)="boxclip"
  invisible(clipout)
}


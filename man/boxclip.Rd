\name{boxclip}
\alias{boxclip}
\title{
Boxfilter Main Function
}
\description{
Filters noise from data (e.g. heartrates) using x for x-axis data and y for y-axis, based on the proportion of neighbors of each point in a surrounding box of width and height (these may be determined automatically). It discards all data points that have less than a proportion of clipit neighbors.
}

\usage{
boxclip(x, y, clipit=NULL, QI=NULL, width=NULL, height=NULL,
            miny=10, plotit=TRUE, histo=FALSE)
}

\arguments{
  \item{x}{
  The x-axis of data, a datetime for example. Optional. If x=NULL x<-1:length(y) will be generated.
  }
  \item{y}{
  The y-axis of data, probably noisy. Required.
  }
  \item{clipit}{
  Optional. Y-values with less than a proportion of clipit neighbors will be discarded.
  If clipit is omitted it is set equal to the first trough in the neighbor proportion histogram.
  }ci
  \item{QI}{
  Optional. An integer quality index for each data point.
  }
  \item{width}{
  Optional. The width of the box. If width is omitted it will be generated from floor(length(x)*0.01).
  }
  \item{height}{
  Optional. The height of the box. If height is omitted it will be generated from floor(mean(y,na.rm=T)/4).
  }
  \item{miny}{
  The minimum y-value expected. Defaults to 10. Anything below miny is discarded.
  }
  \item{plotit}{
  Optional.If TRUE show a graph of the original and filtered data.
  }
  \item{histo}{
  Optional. If TRUE also show a histogram of the neighboring points.
  }

}
\details{
Boxfilter mimics the human criterion of self-similarity.
Data points with many neighbors are more trustworthy.
}
\value{

%% boxfilter returns a LIST:
 \item{x}{Original x-axis data}
 \item{y}{Original y-axis data}
 \item{filtered}{Filtered data. Discarded data points are set to NA.}
 \item{neighbors}{Proportion of neighbors of each point.}
}

\author{
Thomas Ruf (thomas.p.ruf@me.com)
}
\note{
To store only filtered data, use e.g.:

  bc=boxclip (x,y)
  data=data.frame(x=bc$x, hrf=bc$filtered)
  data=na.omit(data)
  write.csv(data,file="myheartrates.csv")
}

\seealso{
clipview
}

\examples{

data("wb_month")
data("ibex_hr")

x=wb_month$x
y=wb_month$hr


myclip=boxclip(x,y, histo=TRUE)
summary(myclip)

r=seq(1,28400,by=4) %% to make the example faster
myclip=boxclip(ibex_hr$Time[r],ibex_hr$Heartrate[r],0.65)
summary(myclip)
#store(myclip)


data("sleepduration")
Date=as.POSIXct(sleepduration$Date)
Duration=as.numeric(sleepduration$Bedtime)
boxclip(Date,Duration,miny=0)
}

 \keyword{ "manip" }

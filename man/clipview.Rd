\name{clipview}
\alias{clipview}

\title{
Get View Of Different clipit  Values
}
\description{
Gives a view of the original data, a histogram of the neighbors, and results of four values of clipit. The cutoff-values clipit are determined  by a sequence of length .-
}
\usage{
clipview(x, y, clipit = NULL,width = NULL, height = NULL, miny=10)
}

\arguments{
  \item{x}{
  The x-axis of data, a datetime for example. Optional. If x=NULL x<-1:length(y) will be generated.
}
  \item{y}{
  The y-axis of data, probably noisy. Required.
}
  \item{clipit}{
  This function requires a sequence of four items <1, e.g. clipit=seq(0.1, 0.4, by=0.1)
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
}
\details{
Note that a sequence is required for clipit here, while a scalar is required in boxclip().
}
\value{
  There is no return value.Six graphs are generated, original, histogram, and four for each value of clipit.
}
\author{
Thomas Ruf (thomas.p.ruf@me.com)
}

\seealso{
boxclip()
}
\examples{

data("wb_year.RData")


r=seq(1,54179,by=5)   %% make example faster

x=wb_year$x[r]
y=wb_year$y[r]

clipview(x,y,clipit=seq(0.2,0.5,0.1))

}

 \keyword{ manip }

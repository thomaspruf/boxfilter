\name{store}
\alias{store}

\title{
Store data
}
\description{
Stores original and filtered data.
}
\usage{
store(object)
}
\arguments{
  \item{object}{
Must be of class "boxclip", resulting from boxclip.
}
}
\value{
returns nothing

}
\author{
Thomas Ruf (thomas.p.ruf@me.com)
}

\examples{
data ("wb_month.RData")

x=wb_month$x
y=wb_month$hr

myclip=boxclip(x,y,clipit=0.25,width=15)
summary(myclip)
store(myclip)

}

\keyword{ manip}


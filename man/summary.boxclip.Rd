\name{summary.boxclip}
\alias{summary.boxclip}

\title{
Summarize boxclip results
}
\description{
Summary method for class boxclip.
}

\usage{
\method{summary}{boxclip}(object,...)
}

\arguments{
  \item{object}{
an object of class boxclip
}
  \item{...}{
currently, no other arguments are required.
}
}

\value{
summary.boxclip prints the following items:

\item{clipit}{Cut-off value. Only points with a proportion >= clipit will be retained.}
\item{width}{Rectangle width in x-units. Filter criterion is the proportion of data points inside the rectangle.}
\item{height}{Rectangle height in y-units. Filter criterion is the proportion of data points inside the rectangle.}
\item{full}{Number of original data.}
\item{remaining}{Number of data remaining.}
\item{rest}{Percentage remaining.}
}
\references{
Signer (2010) <doi: 10.1111/j.2041-210X.2009.00010.x>
}
\author{
Thomas Ruf \email{thomas.p.ruf@me.com}
}

\seealso{
\code{\link{boxclip}}
}
\examples{
data ("wb_month.RData")

x=wb_month$x
y=wb_month$hr

myclip=boxclip(x,y,clipit=0.25,width=15)
summary(myclip)
}

\keyword{ manip }

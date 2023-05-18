\name{showdata}
\alias{showdata}

\title{
Boxfiler function
}
\description{
Shows a graph of the data and its change over time.Called by boxclip()
}
\usage{
showdata(x,y)
}


\details{
Asks the user whether to contnue or not.Continue only if xouu want ponis to be deleted
}

\arguments{
  \item{x}{
  The x-axis of data, a datetime for example.
  }
  \item{y}{
  The y-axis of data, probably noisy.
  }
}

\author{
Thomas Ruf (thomas.p.ruf@me.com)
}


\examples{
set.seed(1234)
y=runif(1000,20,30)
ix=sample(1:1000,50)
y[ix]=runif(50,60,70)
showdata(1:1000,y)
}


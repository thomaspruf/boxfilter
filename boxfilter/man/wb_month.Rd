\name{wb_month}
\alias{wb_month}
\docType{data}
\title{
One month of heart rates and their quality in a wild boar. Quality was assessed by Star-Oddi, Island.
}
\description{
Heart rates were obtained from DST centi- HRT, Star-Oddi, Gardabaer, Iceland.
}
\usage{data("wb_month")}
\format{
  A data frame with 3720 observations on the following 3 variables.
  \describe{
    \item{\code{x}}{x-axis, datetime}
    \item{\code{hr}}{y-axis, heart rate}
    \item{\code{QI}}{quality index (0-3) of the signal}
  }
}
\examples{
data(wb_month)
}
\keyword{datasets}

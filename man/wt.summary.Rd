\name{wt.summary}
\alias{wt.summary}
\title{Weighted mean and variance}
\usage{
  wt.summary(x, w, na.rm = FALSE)
}
\description{
  produce weighted mean and variance estimate from a
  numeric vector of values and weights. This function is
  from the R mailing list at
  https://stat.ethz.ch/pipermail/r-help/2008-July/168762.html
}
\examples{
## from example section in ?weighted.mean
## GPA from Siegel 1994
wt <- c(5,  5,  4,  1)/15
x <- c(3.7,3.3,3.5,2.8)
summary(x)
wt.summary(x,wt)
}
\author{
  Gavin Simpson
}


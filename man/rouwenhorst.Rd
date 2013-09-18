\name{rouwenhorst}
\alias{rouwenhorst}
\title{rouwenhorst discretization for AR1}
\usage{
  rouwenhorst(rho, sigma, mu = 0, n)
}
\arguments{
  \item{rho}{first order autocorrelation}

  \item{sigma}{standard deviation of error term}

  \item{mu}{mean of error term}

  \item{n}{number of points to use in approximation}
}
\value{
  list with Pmat (transition matrix) and zgrid (grid
  points)
}
\description{
  translation of
  \url{http://www.karenkopecky.net/rouwenhorst.m}
}
\references{
  \url{http://www.karenkopecky.net/RouwenhorstPaperFinal.pdf}
}


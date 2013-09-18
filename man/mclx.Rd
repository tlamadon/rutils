\name{mclx}
\alias{mclx}
\title{Clustered Regression standard errors for two way clustering}
\usage{
  mclx(fm, dfcw, cluster1, cluster2)
}
\arguments{
  \item{fm}{fitted model}

  \item{fdcw}{degree of freedom correction weights. set 1
  if none required.}

  \item{cluster1}{name of clustering variable 1}

  \item{cluster2}{name of clustering variable 2}
}
\value{
  a coeftest object
}
\description{
  Clustered standard errors similar to
  \link{http://www.stata.com/help.cgi?vce_option} I
  installed a vignette in /inst. Contacted the author
  whether he would work on a package himself, said no.
  Copyright Mahmood Arai, Jan 26, 2008. Here's a vignette:
  \url{http://people.su.se/~ma/clustering.pdf}. Caution:
  not properly tested.
}
\author{
  Mahmood Arai <mahmood.arai@ne.su.se>
}
\references{
  Cameron, Gelbach and Miller (2006)
  \url{http://www.nber.org/papers/t0327}, Arellano (1987)
  \url{http://ideas.repec.org/a/bla/obuest/v49y1987i4p431-34.html}
}


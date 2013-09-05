\name{getNormCop}
\alias{getNormCop}
\title{generates a Normal transition matrix using Gaussian copula}
\usage{
  getNormCop(rho, n, Qn = seq(1/n, 1 - 1/n, l = n),
    cond = FALSE)
}
\arguments{
  \item{rho}{correlation for Normal copula}

  \item{n}{dimensiosn of the matrix}

  \item{Qn}{support, has to be in [0,1] and with length n}

  \item{cond}{whether to return a conditional matrix}
}
\description{
  generates a Normal transition matrix using Gaussian
  copula
}


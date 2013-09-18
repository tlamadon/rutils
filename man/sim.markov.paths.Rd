\name{sim.markov.paths}
\alias{sim.markov.paths}
\title{simulate n paths of length ntime from markov transition matrix}
\usage{
  sim.markov.paths(n, ntime, trans, init = NULL)
}
\arguments{
  \item{trans}{transition matrix}

  \item{ntime}{number of periods}

  \item{n}{number of individuals}

  \item{init}{initial distribution over states at time 1.
  if null, start at stationary distribution.}
}
\value{
  the stationary distribution of G
}
\description{
  simulate n paths of length ntime from markov transition
  matrix
}


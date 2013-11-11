\name{multiplot}
\alias{multiplot}
\title{puts several plots in the same figure}
\usage{
  multiplot(..., plotlist = NULL, file, cols = 1,
    layout = NULL)

  multiplot(..., plotlist = NULL, file, cols = 1,
    layout = NULL)
}
\description{
  puts several plots in the same figure

  Multiple plot function ggplot objects can be passed in
  ..., or to plotlist (as a list of ggplot objects) - cols:
  Number of columns in layout - layout: A matrix specifying
  the layout. If present, 'cols' is ignored. If the layout
  is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  then plot 1 will go in the upper left, 2 will go in the
  upper right, and 3 will go all the way across the bottom.
}
\author{
  winston <winston@stdout.org>
}
\references{
  http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)
}


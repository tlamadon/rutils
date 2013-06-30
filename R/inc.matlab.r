#' spread an array along a given dimension (similar to repmat in Matlab or spread in Fortran)
#' what dim specifies in the order given
#' spread(A , c(2) , 10) will insert a dimension  in the second index of size 10
#' @export
spread <- function(A, loc, dims) {

  if (!(is.array(A))) {
    A = array(A,dim=c(length(A)))
  }

  adims = dim(A)
  
  # check dimensions
  l = length(loc)
  if (max(loc)> length(dim(A))+l) { 
    stop('incorrect dimensions in spread')
  }

  # total dimension not in order
  sdim = c(dim(A),dims)

  # permutation of dims
  edim = c()
  oi =1        # original dims
  ni =length(dim(A))+1 # new dims
  for (i in c(1: (length(dim(A))+l))) {
    if (i %in% loc) {
      edim = c(edim,ni)
      ni = ni+1
    } else {
      edim = c(edim,oi)
      oi = oi +1
    }
  }

  return( aperm( array(A,dim=sdim),edim)) 
}


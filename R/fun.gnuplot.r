
#' creates a gnuplot object
#' @export
#' @examples
#' gnp <- gnuplot()
#' X = spread(seq(0,1,l=20),2,20)
#' Y = spread(seq(0,1,l=20),1,20)
#' gnp + gp_mesh(X*Y)
#' close(gnp)
gnuplot <- function() {

  r = list()
  if (!file.exists('.tmp.gp.output.fifo'))  system('mkfifo .tmp.gp.output.fifo');
  r$p_out <- fifo('.tmp.gp.output.fifo', 'r')
  r$p_in  <- pipe('export GNUTERM=x11; /Applications/Gnuplot.app/Contents/Resources/bin/gnuplot &> output.fifo', 'w')
  class(r) <- 'gnuplot'
  return(r)
}

#' closes a gnuplot object
#' @export
#' @method close gnuplot
close.gnuplot <- function(gp) {
  close(gp$p_out)
  close(gp$p_in)
}

#' allows to pipe commands to gnuplot object
#' @export
gp_pipe <- function(str) {
  r = list(str=str)
  class(r) <- 'gp_pipe'
  return(r)
}

#' generates a 3D mesh from matrix
#' @export
gp_mesh <- function(M) {
  r = list(M=M)
  class(r) <- 'gp_mesh'
  return(r)
}

#' @export
'+.gnuplot' <- function(gp,b) {
  if (class(b) == 'gp_pipe') {
    writeLines("splot '.tmp.mat.dat' matrix with lines", gp$p_in)
    flush(gp$p_in)
    print(readLines(gp$p_out))
  }
  
  if (class(b) == 'gp_mesh') {
     require(MASS)
     write.matrix(b$M, file = ".tmp.mat.dat", sep = "\t")
     writeLines("set pm3d", gp$p_in)     
     writeLines("set hidden3d", gp$p_in)     
     writeLines("splot '.tmp.mat.dat' matrix with lines", gp$p_in)
     flush(gp$p_in)
     print(readLines(gp$p_out))
  }
}



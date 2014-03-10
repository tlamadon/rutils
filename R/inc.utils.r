

#' merges 2 data.frame using a formula
#' example of formula is var1 + var2 | key1 + key2
#' @export
ddmergev <- function(data1,data2,form,verbose=FALSE) {
	
	expr = substitute(form)	

  # make sure we are using data.frame, data.table creates problems
  nns <- names(data1); data1 = data.frame(data1); names(data1)<-nns;
  nns <- names(data2); data2 = data.frame(data2); names(data2)<-nns;
	
	# constructing list of variables
	LHS = expr[[2]]
	RHS = expr[[3]]
  select_rule = parseSumDivide(RHS)
  rename_rule = parseSumDivide(LHS)

  missing_cols = setdiff(c(select_rule$b,rename_rule$a) , names(data2)) 


	# renaming data2
  # --------------
  nn = names(data2)
  for (i in 1:length(rename_rule$a)) {
    I <- which(nn==rename_rule$b[[i]])
    if  (length(I)>0) {
      nn[I] <- rename_rule$a[[i]]
      if (verbose) cat(paste('renaming in data2 ',nn[I],'->', rename_rule$a[[i]]));
    }
  }
  names(data2) <- nn

  # dropping other variables
  # ------------------------
  # checking that the vars are in the data.frame
  missing_cols = setdiff(c(select_rule$b,rename_rule$a) , names(data2)) 
  if (length(missing_cols)>0) stop(paste('data2 does not have columns: ' ,missing_cols,collapse=',') );
  data2 = data2[,c(select_rule$b,rename_rule$a)]

  # merging 
  # ------------------------
  missing_cols = setdiff(c(select_rule$a) , names(data1)) 
  if (length(missing_cols)>0) stop(paste('data1 does not have columns: ' ,missing_cols,collapse=',') );
  r = merge(data1,data2,by.x=select_rule$a, by.y = select_rule$b) 

  return(r)
}

#' useful to quickly profile code
#' @export
ticker <- function() {
  r = list()
  r$start = NA
  r$loops = 0
  r$times = list()
  r$last_pass = proc.time()[[3]]
  class(r) ='ticker'
  return(r)
}


#' useful to quickly profile code
#' @export
loop.ticker <- function(ticker,name) {

  etime =  proc.time()[[3]] - ticker$last_pass
  if (name %in% names(ticker$times)) {
    ticker$times[[name]] = ticker$times[[name]] + etime
  } else {
    ticker$times[[name]] = etime
  }
  ticker$last_pass = proc.time()[[3]]
  return(ticker)
}

#' puts several plots in the same figure
#' @export
multiplot <- function(..., plotlist=NULL, cols) {
    require(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # Make the panel
    plotCols = cols                       # Number of columns of plots
    plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols

    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
    vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
        curRow = ceiling(i/plotCols)
        curCol = (i-1) %% plotCols + 1
        print(plots[[i]], vp = vplayout(curRow, curCol ))
    }

}

#' computes normalized distance between 2 vectors
#' error will recover the context. The next time, the error is back to normal
#' and the context is not recovered
#' @export
dist <- function(M1,M2,type=0) {
  return( mean(  abs(M1-M2)) / mean(pmax(abs(M1),abs(M2))))
}


#' allows to recover the error once! just call recover_once() and the
#' error will recover the context. The next time, the error is back to normal
#' and the context is not recovered
#' @export
recover_once <- function() {
  options(error=function() {
    options(error=NULL)
    recover()
  })
}

parseSumDivide <- function(e) {

  # if we have a symbole we attach it to both lists
  if (is.symbol(e)) {
    return( list(a=c(paste(e)),b=paste(e)))
  }

  # if we have a divide, we split it
  if (e[[1]]==':') {
    return( list(a=c(paste(e[[2]])),b=paste(e[[3]])))
  }

  # if we have a plus, we parse it, and append a and b
  if (e[[1]]=='+') {
    r1 = parseSumDivide(e[[2]])
    r2 = parseSumDivide(e[[3]])
    return(list(a = c(r1$a,r2$a) , b = c(r1$b,r2$b)))
  }
}





#' Clustered Regression standard errors for one way clustering
#'
#' Clustered standard errors similar to \link{http://www.stata.com/help.cgi?vce_option}
#' I installed a vignette in /inst. Contacted the author whether he would work on a package
#' himself, said no. Copyright Mahmood Arai, Jan 26, 2008. Here's a vignette:
#' \url{http://people.su.se/~ma/clustering.pdf}. Caution: not properly tested.
#' @author Mahmood Arai <mahmood.arai@@ne.su.se>
#' @param fm fitted model
#' @param fdcw degree of freedom correction weights. set 1 if none required.
#' @param cluster name of clustering variable
#' @export
#' @return a coeftest object
clx <- function(fm, dfcw, cluster){
	 
	 # reweighting the var-cov matrix for the within model
	 M <- length(unique(cluster))   
	 N <- length(cluster)           
	 K <- fm$rank                        
	 dfc <- (M/(M-1))*((N-1)/(N-K))  
	 uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
	 vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)*dfcw
	 return(coeftest(fm, vcovCL) )
}

#' Clustered Regression standard errors for two way clustering
#'
#' Clustered standard errors similar to \link{http://www.stata.com/help.cgi?vce_option}
#' I installed a vignette in /inst. Contacted the author whether he would work on a package
#' himself, said no. Copyright Mahmood Arai, Jan 26, 2008. Here's a vignette: 
#' \url{http://people.su.se/~ma/clustering.pdf}. Caution: not properly tested.
#' @author Mahmood Arai <mahmood.arai@@ne.su.se>
#' @param fm fitted model
#' @param fdcw degree of freedom correction weights. set 1 if none required.
#' @param cluster1 name of clustering variable 1
#' @param cluster2 name of clustering variable 2
#' @export
#' @references Cameron, Gelbach and Miller (2006) \url{http://www.nber.org/papers/t0327},
#' Arellano (1987) \url{http://ideas.repec.org/a/bla/obuest/v49y1987i4p431-34.html}
#' @return a coeftest object
mclx <- function(fm, dfcw, cluster1, cluster2){
         
	cluster12 = paste(cluster1,cluster2, sep="")
	 M1      <- length(unique(cluster1))
	 M2      <- length(unique(cluster2))
	 M12     <- length(unique(cluster12))
	 N       <- length(cluster1)
	 K       <- fm$rank
	 dfc1    <- (M1/(M1-1))*((N-1)/(N-K))
	 dfc2    <- (M2/(M2-1))*((N-1)/(N-K))
	 dfc12   <- (M12/(M12-1))*((N-1)/(N-K))
	 u1j     <- apply(estfun(fm), 2, function(x) tapply(x, cluster1,  sum))
	 u2j     <- apply(estfun(fm), 2, function(x) tapply(x, cluster2,  sum))
	 u12j    <- apply(estfun(fm), 2, function(x) tapply(x, cluster12, sum))
	 vc1     <- dfc1*sandwich(fm, meat=crossprod(u1j)/N )
	 vc2     <- dfc2*sandwich(fm, meat=crossprod(u2j)/N )
	 vc12    <- dfc12*sandwich(fm, meat=crossprod(u12j)/N)
	 vcovMCL <- (vc1 + vc2 - vc12)*dfcw
	 return( coeftest(fm, vcovMCL) )
}



#' linear mapping from [a,b] to [0,c]
#'
#' @param x ordered array. a grid.
#' @param new.up new upper bound
#' @param plotit boolean true if plot required
#' @export
linear.map <- function(x,new.up,plotit=FALSE) { 
	# maps x \subset [down,up] into [0,new.up]. 
	x <- x[order(x)]
	n <- length(x)
	rval <- (x - x[1])/(x[n] - x[1])*new.up
	if (plotit) plot(x=x,y=rval,main=paste("linear mapping from [",paste(range(x),collapse=","),"] into [0",new.up,"]",sep=""))
	return(rval)
}



#' nonlinear mapping from [a,b] to [0,c]
#'
#' @param x ordered array. a grid.
#' @param new.up new upper bound
#' @export
nonlinear.map <- function(z,low,high,type){
    if (type==1) rval <- ((low + (z-z[1] )/(1 + (z-z[1])/high)))
    if (type==2) rval <- ((low + (z-z[1])^0.6 )/(high + (z-z[1])^0.6))
    if (type==3) rval <- (high*(low + exp(z))/(high + exp(z)))
    if (type==4) rval <- ((low + exp(z))/(1 + (exp(z)/high)))
	return(rval)
}



#' rouwenhorst discretization for AR1
#'
#' translation of \url{http://www.karenkopecky.net/rouwenhorst.m}
#' @references \url{http://www.karenkopecky.net/RouwenhorstPaperFinal.pdf}
#' @param rho first order autocorrelation 
#' @param sigma standard deviation of error term
#' @param mu mean of error term
#' @param n number of points to use in approximation
#' @return list with Pmat (transition matrix) and zgrid (grid points)
#' @export
#' @examples
#' R <- rouwenhorst(rho=0.9,sigma=1.1,mu=0,n=5)
#' print(R$zgrid)	# support points
#' print(R$Pmat)	# transition matrix
#' print(rowSums(R$Pmat))
rouwenhorst <- function(rho,sigma,mu=0,n){
	stopifnot(n>1)
	qu <- (rho+1)/2
	nu <- ((n-1)/(1-rho^2))^(1/2) * sigma
	P  <- matrix(c(qu,1-qu,1-qu,qu),nrow=2,ncol=2)
	if (n>2){
		for (i in 2:(n-1)){
			zeros    <- rep(0,i)
			zzeros   <- rep(0,i+1)
			P        <- qu * rbind(cbind(P,zeros,deparse.level=0),zzeros,deparse.level=0) + (1-qu) * rbind(cbind(zeros,P,deparse.level=0),zzeros,deparse.level=0) + (1-qu) * rbind(zzeros,cbind(P,zeros,deparse.level=0),deparse.level=0) + qu * rbind(zzeros,cbind(zeros,P,deparse.level=0),deparse.level=0)
			P[2:i, ] <- P[2:i, ]/2
		}
	}
	zgrid <- seq(from=mu/(1-rho)-nu,to=mu/(1-rho)+nu,length=n)
	return(list(Pmat=P,zgrid=zgrid))
}



#' Obtain stationary distribution of transition matrix by iteration
#'
#' @param G transition matrix
#' @param g initial distribution
#' @param n maximal iterations
#' @return the stationary distribution of G
#' @export
stationary.dist <- function(G,g,n){
	for (i in 1:n){
		g <- G %*% g
	}
	return(g)
}


#' simulate n paths of length ntime from markov transition matrix 
#' 
#' @param trans transition matrix
#' @param ntime number of periods
#' @param n number of individuals
#' @param init initial distribution over states at time 1. if null, start
#' at stationary distribution.
#' @return the stationary distribution of G
#' @export
sim.markov.paths <- function(n,ntime,trans,init=NULL){
	if (is.null(init))	init <- stationary.dist(G=trans,g=rep(1/dim(trans)[1],times=dim(trans)[1]),n=1000)	
	out       <- matrix(NA,nrow=n,ncol=ntime)
	out[,1]   <- sample(1:length(init),size=n,replace=T,prob=init)
	for (i in (1:n)){
		for (it in (2:ntime)){
			out[i,it] <- sample(1:length(init),size=1,replace=T,prob=trans[out[i,it-1], ])
		}
	}
	return(out)
}


#' spline knot allocator for square basis functions
#'
#' allocates a vector of spline knots placed according to
#' deBoors average rule such that the resulting matrix of
#' basis functions is square: num of data points equals num
#' of basis functions.
#' @param degree degree of spline
#' @param grid vector of grid points
#' @return spline knot vector
knot.select.old <- function(degree,grid){
# returns a knotvector for a grid of data sites and a spline degree
	grid <- grid[order(grid)]
    n <- length(grid)
    # if (n<(degree+1)*2+1) stop("need at least 2*(degree+1) +1 grid points")
    p <- n+degree+1     # number of nodes required for solving Ax=b exactly
    knots <- rep(0,p)
    knots <- replace(knots,1:(degree+1),rep(grid[1],degree+1))
    knots <- replace(knots,(p-degree):p,rep(tail(grid,1),degree+1))
# this puts multiplicity of first and last knot in order
# if there are anough gridpoints, compute intermediate ones
    if (n<(degree+1)) stop("to few grid points for clamped curve")
    if (n>(degree+1)){
        for (j in 2:(n-degree)) knots[j+degree] <- mean(grid[j:(j+degree-1)])
    }
    return(knots)
}



#' Multiple plot function
#
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' - cols:   Number of columns in layout
#' - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#' @author winston <winston@@stdout.org> 
#' @references http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)
#' @export
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#' Compute Coefficients of Kronecker product approximation
#'
#' computes b = kron(matrixd,kron(...,(kron(matrix2,matrix1))...))*y
#'
#' Given a function f(x1,x2,...,xd), computes the approximating
#' coefficient vector b in
#' fhat(x1,x2,...,xd) = b * B(x1,x2,...xd)
#' where B() is the tensor product of all d univariate 
#' basis functions. Those basis functions can be anything
#' i.e. splines or polynomials etc. Main restriction:
#' the basis functions MUST be square. For the splines,
#' means that you have to choose as many evaluation points
#' as you choose basis functions.
#' @param y numeric vector with function values ordered in the right way.
#' first index should go fastest.
#' @param matrices a list of SQUARE basis functions. one basis function per
#' dimension.
#' @return vector of approximating coefficients.
#' @examples
#' n1 <- 2
#' n2 <- 3
#' n3 <- 3
#' m1 <- matrix(rnorm(n1^2),nrow=n1,ncol=n1)
#' m2 <- matrix(rnorm(n2^2),nrow=n2,ncol=n2)
#' m3 <- matrix(rnorm(n3^2),nrow=n3,ncol=n3)
#' xvals <- expand.grid(m1[,1],m2[,1],m3[,1])
#' func <- function( x ) {
#' 	return( x[1] + x[2] + x[3] + x[1]*x[2] * x[3] )
#' }
#'
#' ## these are the function values
#' y <- apply(xvals,1,func)
#'
#' ## build tensor product matrix
#' kron      <- kronecker(m3,kronecker(m2,m1))
#'
#' ## notice: in kron the indexing corresponds to this:
#' ## uncomment to see
#' ##expand.grid( fastest=1:nrow(m1), middle=1:nrow(m2), slowest=1:nrow(m3) )
#'
#' ## do multiplication
#' ## that is the standard way
#' R.kron <- kron %*% y
#'
#' ##do kron.prod
#' ##that's the deBoer Algo
#' my.kron <- kron.prod(y,list(m1,m2,m3))
#'
#' all.equal(as.numeric(R.kron),my.kron)  #TRUE
kron.prod <- function(y,matrices){
	# INPUT
	# matrices: list(matrix1, matrix2,..., matrixd)
	# 			where dim(matrixj) = c(nj,nj) [SQUARE!]
	# y: value vector 
	# 	 with length(y) = n1*...*nd
	#
	# OUTPUT
	# y1: kron(matrixd,kron(...,(kron(matrix2,matrix1))...))*y
	#
	# check user input
	stopifnot(is.list(matrices))
	# stop if not all of them are square
	allsquare <- lapply(matrices,function(x){dim(x)[1]==dim(x)[2]})
	stopifnot(all(unlist(allsquare)))
	# get sizes
	nmatrices <- length(matrices)
	nall <- length(y)
	nullvec   <- rep(0,nall)
	# compute product for first matrix
	y0    <- y
	stemp <- matrices[[1]]
	n     <- dim(stemp)[1]
	m     <- nall/n
	y1    <- nullvec
	for (i in 1:m){
		y1[m*(0:(n-1)) + i]  <- stemp %*% y0[(n*(i-1)) + (1:n)]
	}
	if (nmatrices > 1){
		# for all other matrices
		for(imat in 2:nmatrices){
			y0    <- y1
			stemp <- matrices[[imat]]
			n     <- dim(stemp)[1]
			m     <- nall/n
			y1    <- nullvec
			for (i in 1:m){
				y1[m*(0:(n-1)) + i]  <- stemp %*% y0[(n*(i-1)) + (1:n)]
			}
		}
	}
	return(y1)
}


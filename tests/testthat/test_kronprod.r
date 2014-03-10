




context("testing kron.prod function")


test_that("kron.prod throws error with non-square matrices",{

	m1 <- matrix(1,nrow=2,ncol=2)
	m2 <- matrix(1,nrow=2,ncol=3)
	y <- rep(0,2*2)

	expect_error( kron.prod(y,list(m1,m2)) )

})

test_that("kron.prod has equal result on 3 dimensions that matrix inversion",{

	n1 <- 2
	n2 <- 3
	n3 <- 3
	m1 <- matrix(rnorm(n1^2),nrow=n1,ncol=n1)
	m2 <- matrix(rnorm(n2^2),nrow=n2,ncol=n2)
	m3 <- matrix(rnorm(n3^2),nrow=n3,ncol=n3)
	xvals <- expand.grid(m1[,1],m2[,1],m3[,1])
	func <- function( x ) {
		return( x[1] + x[2] + x[3] + x[1]*x[2] * x[3] )
	}

	# these are the function values
	y <- apply(xvals,1,func)

	# build tensor product matrix
	kron      <- kronecker(m3,kronecker(m2,m1))

	# notice: in kron the indexing corresponds to this:
	# uncomment to see
	#expand.grid( fastest=1:nrow(m1), middle=1:nrow(m2), slowest=1:nrow(m3) )

	# do multiplication
	# that is the standard way
	R.kron <- kron %*% y

	# do kron.prod
	# that's the deBoer Algo
	my.kron <- kron.prod(y,list(m1,m2,m3))

	expect_true( all.equal(as.numeric(R.kron),my.kron) )
})


	



test_that("kron.prod has equal result on 6 dimensions that matrix inversion",{

	# more dimensions: 6
	n1 <- 2
	n2 <- 5
	n3 <- 4
	n4 <- 3
	n5 <- 3
	n6 <- 3
	m1 <- matrix(rnorm(n1^2),nrow=n1,ncol=n1)
	m2 <- matrix(rnorm(n2^2),nrow=n2,ncol=n2)
	m3 <- matrix(rnorm(n3^2),nrow=n3,ncol=n3)
	m4 <- matrix(rnorm(n4^2),nrow=n4,ncol=n4)
	m5 <- matrix(rnorm(n5^2),nrow=n5,ncol=n5)
	m6 <- matrix(rnorm(n6^2),nrow=n6,ncol=n6)
	y <- runif(n=n1*n2*n3*n4*n5*n6)

	# build tensor product matrix
	kron      <- kronecker(m6,kronecker(m5,kronecker(m4,kronecker(m3,kronecker(m2,m1)))))
	# do multiplication
	R.kron <- kron %*% y
	
	# do kron.prod
	my.kron <- kron.prod(y,list(m1,m2,m3,m4,m5,m6))

	expect_true( all.equal(as.numeric(R.kron),my.kron) )


	})




test_that("kron.prod works where normal method does not",{


	n1 <- 30
	n2 <- 20
	n3 <- 20
	m1 <- matrix(rnorm(n1^2),nrow=n1,ncol=n1)
	m2 <- matrix(rnorm(n2^2),nrow=n2,ncol=n2)
	m3 <- matrix(rnorm(n3^2),nrow=n3,ncol=n3)
	y <- runif(n=n1*n2*n3)

	# build tensor product matrix
	# my computer dies when attempting to build this tensor product

	# run and time kron.prod
	my.kron <- kron.prod(y,list(m1,m2,m3))

	expect_true( length(my.kron) == ncol(m1)*ncol(m2)*ncol(m3) )

	})



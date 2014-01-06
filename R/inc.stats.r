


# mean/sd of wage
mv <- function(v,name) {
  sfit1 = summary(lm(v ~1))
  dev = abs(v - sfit1$coef[1,1])^2
  sfit2 = summary(lm(dev ~1))
  r = data.frame( moment = 'mean' , value=sfit1$coef[1,1],sd=sfit1$coef[1,2])
  r = rbind(r,data.frame( moment = 'var' ,  value=sfit2$coef[1,1],sd=sfit2$coef[1,2]))
  r$name=name
  return(r)
}

#' Variance with standard error
#'
#' compute the variance and returns the standard error
#' on the estimate. It the return value is a data.frame
#' @param x vector fo which the variance needs to be computed
#' @param y is optional, in case you want to compute covariance 
#' @param att is the list of attributes to append to the results
#' @export
var.sd <- function(x,y=c(),att=list()) {
  x = x - mean(x,na.rm=T)
  if (length(y)==0) {
    y = x
  } else {
    y = y - mean(y,na.rm=T)
  }
  sfit1 = summary(lm( x*y ~1))
  att$moment = 'cov'
  att$value  = sfit1$coef[1,1]
  att$sd     = sfit1$coef[1,2]
  att = data.frame(att)
  return(att)
}


means <- function(x) {
  sfit = summary(lm(x~1))
  return(data.frame(mean=sfit$coef[1],sem=sfit$coef[2],pval=sfit$coef[4],sd=sd(x)))
}

wt.summary <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
      w <- w[i <- !is.na(x)]
      x <- x[i]
  }
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)

  return(list(mean=mean.w,var= (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm =na.rm)))
}

wt.cor <- function(x,y,w) {
 # we first compute the rank
 x <- rank(x) / (length(x) + 1)  ## pseudo-observations
 y <- rank(y) / (length(y) + 1)  ## pseudo-observations
 
 r  = cov.wt( cbind(x,y),w,cor=TRUE)
 return(r$cor[2,1])
}

#' generates a Normal transition matrix using Gaussian copula
#' @param rho correlation for Normal copula
#' @param n dimensiosn of the matrix
#' @param Qn support, has to be in [0,1] and with length n
#' @param cond whether to return a conditional matrix
#' @export
getNormCop <- function(rho,n,Qn= seq(1/n,1-1/n,l=n),cond=FALSE) {

  cop = normalCopula(rho) 

  #create the grid
  vals = expand.grid(p=Qn,p2=Qn)
  
  # apply the copula
  vals$v = dCopula(as.matrix(vals),cop)
  G = array(vals$v,dim=c(n,n)) 

  # making it conditional
  if (cond) {
    G = t(apply(G,1,function(v) { return(v/sum(v)) }))
  }

  return(G)
}

getNormPdf <- function(sigma, n) {
  Z  = as.array(qnorm(seq(1/n , 1-1/n,l=n),sd=sigma))
  G  = as.array(dnorm(Z),sd=sigma); G=G/sum(G)
  return(list(vals=Z,mass=G))
}


# copied from MSBVAR
rmultnorm <- function (n, mu, vmat, tol = 1e-10) 
{
    p <- ncol(vmat)
    if (mu==0) mu = rep(0,p);

    if (length(mu) != p) 
        stop(paste("mu vector is the wrong length:", length(mu)))
    vs <- La.svd(vmat)
    vsqrt <- t(t(vs$vt) %*% (t(vs$u) * sqrt(vs$d)))
    ans <- matrix(rnorm(n * p), nrow = n) %*% vsqrt
    ans <- sweep(ans, 2, mu, "+")
    dimnames(ans) <- list(NULL, dimnames(vmat)[[2]])
    ans
}

sem <- function(x,...) {
  return(sd(x,...)/sqrt(length(x)))
}


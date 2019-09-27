#' My ci for E(l)
#'
#' This Function creates confidence intervals for any linear combination of betas
#'
#' @param Y dependent data
#' @param X Design Matrix
#' @param alpha confidence level
#' @param a l=a'B
#' @param l l=a'B
#'
#' @return ci, scatterplots of data
#' @export
#'
#' @examples
#' ci=myl(Y,X,alpha,a,l)
myl<-function(Y,X,alpha,a,l){

  n=nrow(X)
  k=ncol(X)-1
  t=qt(1-alpha/2,n-(k+1))
  pm=c(-1,1)

  model=lm(Y~X[,-1])
  coeff=model$coefficients

  betahat=matrix(data=coeff,nrow=k+1,ncol=1)

  coeff=model$coefficients
  r=model$residuals
  SSE=sum(r^2)
  s=sqrt(SSE/(n-(k+1)))

  ci=l+pm*t*s*sqrt(t(a)%*%solve(t(X)%*%X)%*%a)

  pairs(X)
  return(list(ci=ci))

}

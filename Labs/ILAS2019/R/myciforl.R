#' Confidence interval for l=a'beta
#'
#' This function gives confidence intervals for a multiple regression model, where l=a'*beta. Meaning l is any linear combination of beta parameters, where
#' Y=beta_0+beta_1*x_1+...
#'
#' @param alpha confidence level
#' @param betahat point estimates for beta, where Y=beta_0+X_1*beta_1+...
#' @param X design matrix nx(k+1) where the first column is all 1s (for beta_0)
#' @param a vector with linear combination terms for beta
#' @param s estimate of the standard deviation
#'
#' @return confidence interval for l
#' @export
#'
#' @examples
#' ci=myCIforl(alpha,betahat,X,a,s)
myCIforl=function(alpha,betahat,X,a,s){
  plusminus=c(-1,1)
  n=nrow(X)
  k=ncol(X)-1

  ci=t(a)%*%betahat+plusminus*qt(1-alpha/2,n-(k+1))*s*sqrt(t(a)%*%solve(t(X)%*%X)%*%a)

  return(list(ci=ci))
}

#' summary
#'
#' Returns confidence intervals for beta hat
#'
#' confidence intervals are derived using the bootstrap percentile interval given in
#' http://statweb.stanford.edu/~owen/courses/305-1314/FoxOnBootingRegInR.pdf
#' Basically we're just taking the quantiles of our iter beta hat estimates
#'
#'
#' @param x this is the output of the constructor function
#'
#' @return bootstrap confidence intervals for betahat estimates
#' @export
#'
#' @examples
#' summary(obj)
summary.DABB5734boot=function(x,alpha=0.05){

  n=length(x[[1]]$betahat) #number of beta params
  niter=length(x) #number of bootstrap samples

  ci=matrix(NA,nr=2,nc=n) #nrows is 2 for lower and upper, ncols is number of betas

  betanames=c()
  for(i in 1:n){
    betanames[i]=paste("beta",toString(i-1))
    ests=purrr::map_dbl(x,~.x$betahat[i]) #iter beta i hat estimates
    ci[,i]=quantile(ests,probs=c(alpha/2,1-alpha/2))
  }

  rownames(ci)=c("lower","upper")
  colnames(ci)=betanames


  return(list(ci=ci))

}

#' My ci for new y
#'
#' @param Y
#' @param X
#'
#' @return yhat, ci, pairs plot, ci plot
#' @export
#'
#' @examples list=myy(Y,X)
myy=function(Y,X){
  n=nrow(X)
  k=ncol(X)-1
  model=lm(Y~X[,-1])
  r=model$residuals
  coeff=model$coefficients

  SSE=sum(r^2)
  s=sqrt(SSE/(n-(k+1)))
  t=qt(1-alpha/2,n-(k+1))
  pm=c(-1,1)

  ci_matrix=matrix(,nrow=n,ncol=2)
  yhat_vector=c()
  for(i in 1:n){
    x=X[i,]

    yhat=0
    for(j in 1:k+1){
      yhat=yhat+(coeff[j]*x[j])
    }

    ci=yhat+pm*t*s*sqrt(1+t(x)%*%solve(t(X)%*%X)%*%x)
    ci_matrix[i,1]=ci[1]
    ci_matrix[i,2]=ci[2]
    yhat_vector=append(yhat_vector,yhat)

  }



  ##### Pairs Plot

  pairs_data=X
  pairs_data[,1]=Y

  pairs(pairs_data)

  ###### plot of cis and point estimates (x=1:n)
  index=seq(n)
  plot(index,yhat_vector,ylim=c(min(ci_matrix[,1]),max(ci_matrix[,2])))
  for(i in 1:n){
    segments(x0=i,y0=ci_matrix[i,1],y1=ci_matrix[i,2])
  }


  names(yhat_vector)=seq(n)
  return(list(yhat=yhat_vector,ci=ci_matrix))
}

#' Plot
#'
#' Histogram of beta hat estimates for iter bootstrap samples,
#' also plots vertical lines for the 100(1-alpha) percent confidence interval
#'
#'
#' @param x this is the output of the constructor function
#' @param betaindex index of beta hat estimate, starts at 0 for beta 0 hat
#'
#' @return plots of beta hat estimates for bootstrap samples
#' @export
#'
#' @examples
#' obj=constructor(mtcars,yterm="mpg",xterms="disp+hp")
#' plot(obj)
plot.DABB5734boot=function(x,betaindex=1,alpha=0.05){
  require(ggplot2)

  bi=purrr::map_dbl(x, ~.x$betahat[betaindex+1]) #iter beta i hat estimates

  #lower and upper quantiles for plotting error estimates
  quantiles=quantile(bi,probs=c(alpha/2,1-alpha/2))

  idx=toString(betaindex)
  title=paste("Beta",idx," Hat Estimates for Bootstrap Samples")

  g=ggplot(as.data.frame(bi), aes(x=bi)) + geom_histogram(aes(fill = "pink"))
  g=g+ xlab(paste("Beta",idx)) + ggtitle(title) + theme(legend.position = "none")
  g=g+geom_vline(xintercept=quantiles[1],linetype="dashed", color = "blue")
  g=g+geom_vline(xintercept=quantiles[2],linetype="dashed", color = "blue")
  print(g)

}

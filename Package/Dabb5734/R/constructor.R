#' Constructor
#'
#' This is an object oriented implementation of bootstrap on MLR data. This function creates iter bootstrap samples
#'
#' Useful to utilize this package when assumptions of MLR are blown. If our sample has size n, the basic process
#' is take a bunch of samples of size n, with replacement. You can then look at the distribution of sample statistics
#' like mean and standard deviation. This treats the sample as an estimate of the population. A useful
#' examination of bootstrapping regression models can be found at
#' http://statweb.stanford.edu/~owen/courses/305-1314/FoxOnBootingRegInR.pdf
#' A key quote from that link: The population is to the sample as the sample is to the bootstrap samples.
#' @param df data frame for MLR
#' @param yterm y variable name
#' @param xterm x variable names
#'
#' @return constructor to be used with other methods of this package
#' @export
#'
#' @examples
#' obj=constructor(mtcars,yterm="mpg",xterms="disp+hp")
constructor=function(df,yterm,xterms,iter=1000){
  require(purrr)

  oneboot=bootstrap(df,yterm,xterms)
  formula_name=oneboot$formula
  print(paste0("Formula is ",formula_name[2],formula_name[1],formula_name[3],collapse=" "))

  out=map(1:iter, ~bootstrap(df,yterm,xterms))

  class(out)="DABB5734boot"
  return(invisible(out))
}

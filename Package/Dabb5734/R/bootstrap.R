#' Bootstrap
#'
#' This function generates one bootstrap sample
#'
#' @param df data frame
#' @param yterm y variable name
#' @param xterms x variable names
#'
#' @return bootstrapped data frame, summary lm object, betahat estimates, formula used.
#' @export
#'
#' @examples
#' bootstrap(mtcars,yterm="mpg",xterms="disp+hp")
bootstrap=function(df,yterm,xterms){
  dfb=df[sample(nrow(df), replace = TRUE),,drop = FALSE] #create bootstrapped sample, size n, sampled with replacement from df
  mod = formula(paste0(yterm,"~", xterms)) #create formula for lm
  lmb = lm(mod, dfb) #create lm object
  lmsum=summary(lmb) #summary lm object
  betahat = lmsum$coeff[,"Estimate"] #betahat values
  outb=list(dfb = dfb, lmsum = lmsum, betahat=betahat,formula=mod)

}

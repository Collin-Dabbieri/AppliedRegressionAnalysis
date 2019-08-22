---
title: 'Mini-Lab: CI for $p_i$'
author: "Collin Dabbieri"
date: "2019-08-22 18:41:44"
output: 
  html_document:
    toc: yes
    toc_float: yes
    keep_md: yes
---



# Introduction

Usually statisticians make the assumption that when $n\rightarrow \infty$ we can say

$$V(\hat{P}_i)\approx \frac{\hat{p}_i(1-\hat{p}_i)}{n}$$

So that a $100(1-\alpha)\%$ ci for $p_i$ is

$$\hat{p_i}\pm Z_{\alpha/2}\sqrt{\frac{\hat{p}_i(1-\hat{p}_i)}{n}}$$


# Tasks


1. Please use the theory given in class and complete what I gave (partly done - no continuity correction) to find a more accurate confidence interval by NOT assuming the above for $V(\hat{P}_i)$. Show all working in $\LaTeX$ within this document.

$$\begin{eqnarray} 
P\left(-z_{\alpha/2}\le \frac{\hat{p}_i-p_i}{\sqrt{p_i(1-p_i)/n}}\le z_{\alpha/2}\right)&=&1-\alpha\\

P\left( -z_{\alpha/2} \sqrt{\frac{p_i (1-p_i)}{n}} \le \hat{p}_i-p_i \le z_{\alpha/2}\sqrt{\frac{p_i(1-p_i)}{n}}   \right)&=&1-\alpha\\

P\left(z_{\alpha/2}^2\frac{p_i(1-p_i)}{n}\le (\hat{p}_i -p_i)^2\le z_{\alpha/2}^2\frac{p_i(1-p_i)}{n} \right)&=&1-\alpha\\

P\left(z_{\alpha/2}^2\frac{p_i(1-p_i)}{n}=(\hat{p}_i -p_i)^2 \right)&=&1-\alpha \\

P\left(z_{\alpha/2}^2\frac{p_i(1-p_i)}{n} = \hat{p}_i^2 - 2\hat{p}_ip_i +p_i^2   \right)&=&1-\alpha\\

P\left(\frac{z_{\alpha/2}^2p_i}{n}-\frac{z_{\alpha/2}^2p_i^2}{n}-\hat{p}_i^2+2\hat{p}_ip_i-p_i^2  =0\right)&=&1-\alpha\\

P\left((-\frac{z_{\alpha/2}^2}{n}-1)p_i^2+(\frac{z_{\alpha/2}^2}{n}+2\hat{p}_i)p_i-\hat{p}_i^2 = 0  \right)&=&1-\alpha\\

P\left(p_i=\frac{-\frac{z_{\alpha/2}^2}{n}-2\hat{p}_i\pm\sqrt{(\frac{z_{\alpha/2}^2}{n}+2\hat{p}_i)^2-4(-\frac{z_{\alpha/2}^2}{n}-1)(-\hat{p}_i^2)}}{-2(\frac{z_{\alpha/2}^2}{n}+1)}  \right)&=&1-\alpha\\

P\left(p_i=\frac{\hat{p}_i+\frac{z_{\alpha/2}^2}{2n}}{1+\frac{z_{\alpha/2}^2}{n}} \pm \frac{\sqrt{\frac{z_{\alpha/2}^4}{n^2}+4\hat{p}_i\frac{z_{\alpha/2}^2}{n}+4\hat{p}_i^2-4\frac{z_{\alpha/2}^2}{n}\hat{p}_i^2-4\hat{p}_i^2}}{-2(1+\frac{z_{\alpha/2}^2}{n})}  \right)&=&1-\alpha\\

P\left(p_i=\frac{\hat{p}_i+\frac{z_{\alpha/2}^2}{2n}}{1+\frac{z_{\alpha/2}^2}{n}} \pm \frac{\sqrt{\frac{z_{\alpha/2}^4}{n^2}+4\hat{p}_i\frac{z_{\alpha/2}^2}{n}-4\frac{z_{\alpha/2}^2}{n}\hat{p}_i^2}}{-2(1+\frac{z_{\alpha/2}^2}{n})}  \right)&=&1-\alpha\\

P\left(p_i=\frac{\hat{p}_i+\frac{z_{\alpha/2}^2}{2n}}{1+\frac{z_{\alpha/2}^2}{n}} \pm \frac{z_{\alpha/2}\sqrt{\frac{z_{\alpha/2}^2}{n^2}+4\frac{\hat{p}_i}{n}-4\frac{\hat{p}_i^2}{n}}}{-2(1+\frac{z_{\alpha/2}^2}{n})}  \right)&=&1-\alpha\\

P\left(p_i=\frac{\hat{p}_i+\frac{z_{\alpha/2}^2}{2n}}{1+\frac{z_{\alpha/2}^2}{n}} \pm \frac{-2(z_{\alpha/2})\sqrt{\frac{\hat{p}_i(1-\hat{p}_i)}{n}+\frac{z_{\alpha/2}^2}{4n^2}}}{-2(1+\frac{z_{\alpha/2}^2}{n})}  \right)&=&1-\alpha\\

P\left(p_i=\frac{\hat{p}_i+\frac{z_{\alpha/2}^2}{2n}}{1+\frac{z_{\alpha/2}^2}{n}} \pm \frac{z_{\alpha/2}\sqrt{\frac{\hat{p}_i(1-\hat{p}_i)}{n}+\frac{z_{\alpha/2}^2}{4n^2}}}{1+\frac{z_{\alpha/2}^2}{n}}  \right)&=&1-\alpha\\

\end{eqnarray}$$

So a $100(1-\alpha)\%$ ci for $p_i$ is

$$\frac{\hat{p}_i+\frac{z_{\alpha/2}^2}{2n}}{1+\frac{z_{\alpha/2}^2}{n}} \pm \frac{z_{\alpha/2}\sqrt{\frac{\hat{p}_i(1-\hat{p}_i)}{n}+\frac{z_{\alpha/2}^2}{4n^2}}}{1+\frac{z_{\alpha/2}^2}{n}}$$

Help with Latex see <https://www.stat.cmu.edu/~cshalizi/rmarkdown/> also
<https://en.wikibooks.org/wiki/LaTeX/Mathematics>

Help with the final result may be found here: <https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval>

Make an R function that will calculate the more accurate interval as well as the usual approximation. Call it `mycip(x,n,alpha)` the output will be a list.


```r
mycip=function(x,n,alpha){
  phat=x/n
  z=qnorm(1-alpha/2,0,1)
  mp=c(-1,1)
  #less constraining approximation
  ci1=phat+mp*z*sqrt(phat*(1-phat)/n)
  
  #more rigorous version worked out above
  ci2=(phat+z^2/(2*n))/(1+z^2/n)+mp*((z)/(1+z^2/n))*sqrt((phat*(1-phat))/(n)+(z^2)/(4*n^2))
  
  return(list(ci1=ci1,ci2=ci2))
}
```


2. If `x=9,n=20` and $\alpha=0.05$ call `mycip()`


```r
mycip(9,20,0.05)
```

```
## $ci1
## [1] 0.2319678 0.6680322
## 
## $ci2
## [1] 0.2581979 0.6579147
```


3. If `x=4,n=10` and $\alpha=0.05$  call `mycip()` 


```r
mycip(4,10,0.05)
```

```
## $ci1
## [1] 0.09636369 0.70363631
## 
## $ci2
## [1] 0.1681803 0.6873262
```

4. Write a sentence commenting on the width of the intervals calculated with each of the two methods.

In both cases the second method (the one worked out above) provides a confidence interval with a more narrow width, i.e it is a better constraint for $p_i$.

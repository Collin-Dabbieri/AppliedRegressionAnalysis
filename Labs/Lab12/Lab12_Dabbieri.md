---
title: "Laboratory 12"
author: "Collin Dabbieri"
date: "11/14/2019"
output: 
  html_document:
    toc: yes
    toc_float: yes
    keep_md: yes
---



# Task 1


```r
getwd()
```

```
## [1] "/home/collindabbieri/Documents/AppliedRegressionAnalysis/Labs/Lab12"
```

# Task 2

## Explain what a parametric test is:

A parametric test is often one that relies on the assumption that the data are sampled from a normally distributed population. When that assumption holds, parametric tests are the most powerful.



## Explain what a non-parametric test is:

Non-parametric tests do not depend on the distribution of the sampled population. They are distribution-free tests. These tests focus on the location of the probability distribution of the population, rather than on specific parameters of the population, like the mean.




## What is the branch of statistics devoted to "distribution free statistics" called?

Nonparametrics


# Task 3

## Carry out example 15.1 by hand

We have 10 counts of percentages of active bacteria. Want to determine if the median is greater than 40%. Use $\alpha=0.05$

$$H_0:\tau=40$$
$$H_a:\tau>40$$
Our test statistic, S, is the number of $y_i$'s in the sample that exceed 40


```r
library(readxl)

#10 counts of percentage of active bacteria
bacteria=read_excel("../../Dataxls/Excel/BACTERIA.XLS")
bacteria
```

```
## # A tibble: 10 x 1
##    ACTBAC
##     <dbl>
##  1     41
##  2     33
##  3     43
##  4     52
##  5     46
##  6     37
##  7     44
##  8     49
##  9     53
## 10     30
```

S=7.

S has a binomial distribution with n=10, p=0.5

$$p-value=P(x\geq7)=1-P(x\leq6)$$



```r
1-pbinom(6,10,0.5)
```

```
## [1] 0.171875
```


Our p-value is greater than $\alpha$ so we cannot reject the NULL. There is not sufficient evidence to conclude that the median is greater than 40%.


## Use a package to carry out example 15.1


```r
library(signmedian.test)
signmedian.test(c(41,33,43,52,46,37,44,49,53,30),40,alternative="greater")
```

```
## 
## 	Exact sign test
## 
## data:  c(41, 33, 43, 52, 46, 37, 44, 49, 53, 30)
## #(x>40) = 7, mu = 40, p-value = 0.1719
## alternative hypothesis: the median of x is greater than mu
## 97.85156 percent confidence interval:
##  33 52
## sample estimates:
## point estimator 
##            43.5
```

Again our p-value is greater than $\alpha$ so we cannot reject the NULL

# Task 4


## Describe the Wilcoxon rank sum test:

This test tests the NULL hypothesis that the probability distributions associated with the two populations are equivalent against the alternative hypothesis that one distribution is shifted to the right (or left) of the other. It accomplishes this by comparing the rank sums of the two samples.



## Using an R package, perform the analysis of example 15.2



```r
word_rating=c(35,50,25,55,10,30,20)
programmer_rating=c(45,60,40,90,65,85,95)


wilcox.test(word_rating,programmer_rating)
```

```
## 
## 	Wilcoxon rank sum test
## 
## data:  word_rating and programmer_rating
## W = 4, p-value = 0.006993
## alternative hypothesis: true location shift is not equal to 0
```

Our p-value is less than $\alpha=0.05$ so we can say with 95% confidence that the probability distributions have different locations

## Ditto for Example 15.3

Suppose $H_a$: Distribution 2 is shifted to the right of distribution 1.

Locate the rejection region for the test using $\alpha=0.025$


The rank sums come out to be $T_1=32$ and $T_2=73$

Where 1 is for word-processing operators and 2 is for programming specialists. Using $T_1$ as a test statistic, we need to find $T_L$, the lower tailed value of the rank sum for $n_1=n_2=7$. The value is given in Table 15.3 as 37. Therefore we will accept the alternate hypothesis that distribution 2 is shifted to the right of distribution 1 with $\alpha=0.025$ when $T_1\leq37$






---
title: "Laboratory 5"
author: "Collin Dabbieri"
date: "2019-09-23"
output: 
  html_document:
    toc: yes
    toc_float: yes
    keep_md: yes
---



# My functions {.tabset .tabset-pills .tabset-fade}

## Function 1

## Function 2


# Task 1


```r
getwd()
```

```
## [1] "/home/collindabbieri/Documents/AppliedRegressionAnalysis/Labs/Lab5"
```


# Task 2 {.tabset .tabset-pills .tabset-fade}


```r
library(readxl)

clerical=read_excel("../../Dataxls/Excel/CLERICAL.XLS")
head(clerical)
```

```
## # A tibble: 6 x 10
##     Obs Day   `Y-Hours` `X1-Mail` `X2-Gifts` `X3-Charge` `X4-Returns`
##   <dbl> <chr>     <dbl>     <dbl>      <dbl>       <dbl>        <dbl>
## 1     1 M          128.      7781        100         886          235
## 2     2 T          114.      7004        110         962          388
## 3     3 W          147.      7267         61        1342          398
## 4     4 Th         124.      2129        102        1153          457
## 5     5 F          100.      4878         45         803          577
## 6     6 S          119.      3999        144        1127          345
## # … with 3 more variables: `X5-Checks` <dbl>, `X6-Misc` <dbl>,
## #   `X7-Tickets` <dbl>
```

Replace the first 4 entries in the column "X5-Checks" with c(600,500,1000,900) 


```r
print(clerical$`X5-Checks`[1:4])
```

```
## [1]  644  589 1081  891
```

```r
clerical$`X5-Checks`[1:4]=c(600,500,1000,900)
print(clerical$`X5-Checks`[1:4])
```

```
## [1]  600  500 1000  900
```

## A

## B

## C

## D

## E

## F

## G

# Task 3

Using Theorem 11.3 page 574, make a function myl() that produces

-(1-$\alpha$)100% confidence intervals for E(l) given Y, X, $\alpha$, a, l
-a plot using pairs() to show all pairwise plots of the data used in the analysis


```r
myl=function(Y,X,alpha,a,l){
  
  
}
```


Using the modified CLERICAL data with $\alpha=0.05$, and a such that $E(l)=\beta_1-\beta_2$ call your function and place output here





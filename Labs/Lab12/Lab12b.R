#lab 12 pg 841
# sign test
#Data
dird = "E:/MATH4773-5773/DATA/Excel/"
library(readxl)
myread = function(xls)
{
  read_excel(paste(dird,xls,sep=""))
}
# read in data
bact=myread("BACTERIA.xls")
bact
# make vector of data
bact$ACTBAC->act
act
# find sum of TRUes
sum(act-40 > 0)

# Make references to extract -+
ref = (act-40 > 0)*1 +1
ref
c("-","+")[ref]

# calculate pvalue
pval = 1-pbinom(6,10, prob = 0.5)
pval

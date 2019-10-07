# question 1

dird = "E:/MATH4773-5773/DATA/Excel/"
library(readxl)
myread = function(xls)
{
  read_excel(paste(dird,xls,sep=""))
}

#example 11.10 pg 583
CL = myread("CLERICAL.xls")

head(CL)
names(CL)
names(CL) = c("Obs", "Day", "y", "x1", "x2", "x3", "x4", "x5", "x6", "x7")
names(CL)

y.lm = lm(y~x1+x2+x3+x4+x5, data =CL)
summary(y.lm)

plot(residuals(y.lm)~ x1, data = CL)

predict(y.lm)

# Lab 4 MLR MATH 4773
# Data E:\MATH4773-5773\DATA\Excel
dird = "E:/MATH4773-5773/DATA/Excel/"
library(readxl)
myread = function(xls)
{
  f=read_excel(paste(dird,xls,sep=""))
  f
}

#11.6 page 580
bub = myread("BUBBLE2.xls")

head(bub)

y.lm = lm(Diameter ~ MassFlux  +HeatFlux, data = bub)
X = model.matrix(y.lm)

solve((t(X)%*%X))



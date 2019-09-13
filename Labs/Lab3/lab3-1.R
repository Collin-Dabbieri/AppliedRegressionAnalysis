# MATH 4773
# 
# Lab 3
# 
library(readxl)
dird='C:/Users/stew9983/OneDrive - University of Oklahoma/DATAxls/'
library(readxl)
myread=function(x) 
{
  library(readxl)
  read_xls(paste0(dird,x))
}

ins =myread("INSULATION.xls")

ins
y.lm = lm(COMP_Y ~ PRESS_X, data = ins)

library(s20x)
dev.new(noRStudioGD = TRUE)
normcheck(y.lm, shapiro.wilk = TRUE)

model.frame(y.lm)
names(ins) = c("x1", "y")
ins
y.lm = lm(y ~ x1, data = ins)

X = model.matrix(y.lm)
Y = ins[["y"]]
Y
X

x2 = c(1,2,2,4,3) # temp
X = cbind(X, x2)
X

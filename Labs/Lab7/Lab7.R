# Lab 7
# Plotting
# Model Building


library(rgl)

#Data
dird = "E:/MATH4773-5773/DATA/Excel/"
library(readxl)
myread = function(xls)
{
  read_excel(paste(dird,xls,sep=""))
}

#example 12.3 pg 657
prod = myread("PRODQUAL.xls")
head(prod)

# Full second order model 
fit = lm(QUALITY ~ PRESSURE*TEMP + I(PRESSURE^2) + I(TEMP^2), data = prod)
summary(fit)
# This is useful to make the plots
apply(prod, 2 , range)
# We will use this vector to make the 3 dimensional estimated trend
cf = coef(fit)
# Make some plots

t <- seq(80, 100, length = 30) # from range of data
p <- seq(50,60, length = 30)

# estimated trend function
f <- function(t,p) {cf[1] +p*cf[2] + t*cf[3] +p^2*cf[4] + t^2*cf[5] + t*p*cf[6] }
# grid for the plot
q <- outer(t,p, f)
# open graphics area -- there are options!
# This will take any rgl plotting commands
# We can layer graphics on this surface
open3d()
# Change to whatever bg you want
bg3d("white")
material3d(col = "green")
# xyz
persp3d(t, p, q, aspect = c(1, 1, 0.5), col = "lightblue",
        xlab = "Temp", ylab = "Pressure", zlab = "Quality")
# this line is unnecessary 
play3d(spin3d(axis = c(0, 1, 1), rpm = 5), duration = 10)
# add data points
points3d(prod, col = "red", cex = 3)

# Simpler model
fit2 = lm(QUALITY ~ PRESSURE + TEMP, data = prod)
cf2 = coef(fit2)
a <- cf2["TEMP"]
b <- cf2["PRESSURE"]
b
c <- -1
d <- cf2["(Intercept)"]

planes3d(a,b, c, d, alpha = 0.5, col = "Pink")


# Simple with interaction

fit3 = lm(QUALITY ~ PRESSURE*TEMP, data = prod)
cf3 = coef(fit3)

  
  t <- seq(80, 100, length = 30) # from range of data
p <- seq(50,60, length = 30)

g <- function(t,p) {cf3[1] +p*cf3[2] + t*cf3[3] +p*t*cf3[4] }
q <- outer(t,p, g)

open3d()
bg3d("white")
material3d(col = "green")
persp3d(t, p, q, aspect = c(1, 1, 0.5), col = "lightblue",
        xlab = "Temp", ylab = "Pressure", zlab = "Quality")
play3d(spin3d(axis = c(0, 1, 1), rpm = 5), duration = 10)

points3d(prod, col = "red", cex = 3)

prod

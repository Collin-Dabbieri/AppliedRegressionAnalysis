# Lab 5 MLR MATH 4773
# Data E:\MATH4773-5773\DATA\Excel
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
CL[1:4,8 ] <-c(600,500,1000,900)

head(CL)
y.lm = lm(y ~ x1 + x2 + x3, data = CL)
summary(y.lm)
library(s20x)
ciReg(y.lm)
X = model.matrix(y.lm)
dimnames(X)[2]
Y = as.matrix(CL["y"])
class(Y)
dim(Y)
t(Y)
dim(X)[1]
# plotting
# Use ggplot2

library(ggplot2)
windows() # quartz() for mac
g = ggplot(CL, aes(x = x1, y=y, fill = y)) + geom_point(pch =21, size = 4) + geom_smooth(method = "lm", se = TRUE)
g

# Now try 

g = ggplot(CL, aes(x = x1, y=y, fill = Day)) + geom_point(pch =21, size = 4) + geom_smooth(method = "lm", se = TRUE)
g

# also using facets

g = ggplot(CL, aes(x = x1, y=y, fill = Day)) + geom_point(pch =21, size = 4) +
  geom_smooth(method = "lm", se = TRUE) + facet_grid( . ~ Day)
g

mygg = function(X,Y)
{
  graphics.off()
 X = X[,-1]
 nx = dim(X)[2]
 df = cbind(Y,X)
 df = as.data.frame(df)
 library(ggplot2)

 

 xnms = names(df)

 for( i in 1: nx) {
   dev.new(noRStudioGD = TRUE)
   
   g= ggplot(df, aes_string(x=xnms[i+1], y = "y")) + geom_point()
   print(g)
 }
 list(xnames = xnms)
}
mygg(X,Y)

myy = function(Y,X, alpha)
{
  n = dim(X)[1]
  kplus1 = dim(X)[2]
  df = n-(kplus1)
  
  XXinv = solve(t(X)%*%X)
  betahat = (XXinv%*%t(X))%*%Y
  yhat = X%*%betahat
  sse  = t(Y)%*%Y -t(betahat)%*%t(X)%*%Y
  ssq = sse/df
  mat = matrix(NA, nr = n, nc = 3, byrow = TRUE, dimnames = list(1:n,c("L", "U", "yhat") ))
  for(i in 1:n){
    
    a = as.matrix(X[i,])
  mat[i, 1] = yhat[i] - qt(1-alpha/2, df)*sqrt(ssq)*sqrt(1 +t(a)%*%XXinv%*%a)
  mat[i, 2] = yhat[i] + qt(1-alpha/2, df)*sqrt(ssq)*sqrt(1 +t(a)%*%XXinv%*%a)
  mat[i, 3] = yhat[i]
  
  }
  
  list(X=X, Y=Y, ciyhat = mat)
}
# call the function 
obj = myy(Y=Y,X=X,alpha = 0.05)

obj$ciyhat 
windows()
matplot(obj$ciyhat, type = "p", ylab = "Confidence Intervals", xlab = "1:n")

segments(x0 = 1:n, y0 = obj$ciyhat[,1], x1=1:n,y1 = obj$ciyhat[,2])



as.matrix(X[1,])


X
y.lm = lm(Diameter ~ MassFlux  +HeatFlux, data = bub)
summary(y.lm)
X = model.matrix(y.lm)
X
solve((t(X)%*%X))

head(bub)
y.lm2 = lm(Density ~ MassFlux  +HeatFlux, data = bub)
X = model.matrix(y.lm2)
X
inv = solve(t(X)%*%X)
inv

# graphing
# rgl package
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}

library(rgl)
rgl_init()
x = bub$MassFlux
y = bub$HeatFlux
z = bub$Diameter

df = data.frame(x = x, y = y, z = z)

rgl.points(x, y, z, color ="green3", size = 10) # Scatter plot




rgl_add_axes(x, y, z, show.bbox = FALSE)
aspect3d(1,1,1)
# Linear model
fit = lm(z ~ x + y)
coefs <- coef(fit)
coefs
a <- coefs["x"]; b <- coefs["y"]; c <- -1
d <- coefs["(Intercept)"]
rgl.planes(a, b, c, d, alpha=0.2, color = "#D95F02")

#persp3d(x,y,z)
#



# Lab 10
#Chapter 14
#ANOVA

#Task 1
#Example 14.1


sm = sample(1:40, 40, replace =FALSE)
mat = matrix(sm, nc = 4, nr =10, byrow = FALSE)
colnames(mat) = c("A","B","C", "D")
mat
as.data.frame(mat)

# Task 2

set.seed(12);y1 = rnorm(10,14,5)
set.seed(13);y2 = rnorm(10, 15, 5)

f = rep(c(1,2), c(10,10))
f = factor(f,levels=c(1,2))

df = data.frame(y = c(y1,y2), x = f)

dev.new(noRStudioGD = TRUE)
plot(c(y1,y2) ~ as.numeric(f), type = "p", pch =21, bg = "Blue", ylim = c(-5,35))
abline(h = c(mean(y1),mean(y2)))

# library(ggplot2)
# g = ggplot(df, aes(x = f, y = y, fill = f)) + geom_point() 
# g

# Now change the sd
set.seed(12);y1 = rnorm(10,14,10)
set.seed(13);y2 = rnorm(10, 15, 10)

dev.new(noRStudioGD = TRUE)
plot(c(y1,y2) ~ as.numeric(f), type = "p", pch =21, bg = "Red",ylim = c(-5,35))
abline(h = c(mean(y1),mean(y2)))



# Example 14.2



library(rgl)

#Data
dird = "E:/MATH4773-5773/DATA/Excel/"
library(readxl)
myread = function(xls)
{
  read_excel(paste(dird,xls,sep=""))
}


#13.7
oil = myread("OILRIGS.xls")
head(oil)



#example 12.4 pg 663
mossie = myread("MOSQUITO.xls")
head(mossie)

# page 665 12.29
# 
straw = myread("STRAW.xls")

head(straw)

with(straw, cor(DENSITY,DENSITY^2))

u =as.vector(scale(straw[,2]))
u
cor(u,u^2)

strawu = within(straw, {u<-u ; usq <- u^2; Dsq <- DENSITY^2})
strawu
y.lm = lm(TC ~ u + I(u^2), data = strawu)
y.lm2 = lm(TC ~ DENSITY + I(DENSITY^2), data = straw )

windows()
library(s20x)
pairs20x(strawu)

summary(y.lm2)


# Qualitative variables
# Example 12.5 page 669

bid = myread("BIDMAINT.xls")
head(bid)

y.lm = lm(COST ~ STATE , data = bid)
summary(y.lm)
model.matrix(y.lm)

library(ggplot2)
windows()
g = ggplot(bid, aes(x = STATE, y = COST, fill = STATE)) + geom_boxplot()
g

bid2 = within(bid, STATE <- factor(STATE, levels = c("Texas","Kansas","Kentucky")))


y.lm2 = lm(COST ~ STATE, data = bid2)
summary(y.lm2)


# 12.40 page 672
# repellent

rep = myread("REPELLANT.xls")
head(rep)
summary(rep)


y.lm = lm(Cost ~ Type, data = rep)
summary(y.lm)


# Example 12.7 pg 678
cast = myread("CASTINGS.xls")
head(cast)

# Please learn this!
# let R make the dummy variables
# Make "union" the base level
y.lm = lm(CASTINGS ~ INCENTIVE + PLANT + INCENTIVE:PLANT, data = within(cast, PLANT <- factor(PLANT, levels =c("union",'nonunion'))))

summary(y.lm)

# or use the given dummy variables

y.lm2 = lm(CASTINGS ~ INCENTIVE + PDUMMY + INC_PDUM , data = cast)

summary(y.lm2)

cf = coef(y.lm2)
cf

# make a function for plotting
# 
f = function(x1,x2) #x2=0,1
{
  yhat = cf[1] + cf[2]*x1 + cf[3]*x2 + cf[4]*x1*x2
  yhat
}

yh0 = with(cast , f(INCENTIVE,0)) # range of yhat values
yh1 = with(cast , f(INCENTIVE,1))


windows()
minn = min(c(yh0,yh1))
maxx = max(c(yh0,yh1))
curve(f(x,1), xlim = c(20,40), ylim = c(minn, maxx), col ="Red", lwd =3, ylab = "CASTINGS", xlab = "INCENTIVE")
curve(f(x,0), xlim = c(20,40), col ="Blue", lwd =3, add = TRUE)
cat("click graphic to move on")
legend(locator(1), legend = c("union","non union"), fill = c("Blue","Red") )

summary(y.lm)
# Use ggplot2
library(ggplot2)
g = ggplot(cast, aes(x = INCENTIVE, y = CASTINGS, fill = PLANT)) + 
  geom_smooth(method = lm, formula = y ~ x , se = TRUE )
g
# model selection

base = lm(CASTINGS ~ INCENTIVE + PLANT + INCENTIVE:PLANT, data = within(cast, PLANT <- factor(PLANT, levels =c("union",'nonunion'))))


# other ways to use the formula language
base  = lm(CASTINGS ~ .^2, data =cast[,1:3])
model.matrix(base)

library(MASS)

stepAIC(base, scope = list(lower ~ 1, upper  ~ . + INCENTIVE:PLANT), direction = "forward")

# choose model with smallest AIC
library(leaps)
null = lm(CASTINGS~ 1, data = cast)
full = lm(CASTINGS ~ INCENTIVE + PLANT + INCENTIVE:PLANT, data = within(cast, PLANT <- factor(PLANT, levels =c("union",'nonunion'))))
tt =step(null, scope=list(lower=null, upper=full), direction="forward")
step(full, data=cast, direction="backward")

names(tt)

base0 = lm(CASTINGS ~ INCENTIVE + PLANT , data = within(cast, PLANT <- factor(PLANT, levels =c("union",'nonunion'))))

anova(base0,base)




# Full second order model 
fit = lm(QUALITY ~ PRESSURE*TEMP + I(PRESSURE^2) + I(TEMP^2), data = prod)
summary(fit)
# This is useful to make the plots
apply(prod, 2 , range)
# We will use this vector to make the 3 dimensional estimated trend
cf = coef(fit)
cf
# Make some plots

t <- seq(80, 100, length = 30) # from range of data
p <- seq(50,60, length = 30)

# estimated trend function
f <- function(t,p) {cf[1] +p*cf[2] + t*cf[3] +p^2*cf[4] + t^2*cf[5] + t*p*cf[6] }
# grid for the plot
q <- outer(t,p, f)
class(q)
head(q)
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
# formula ax +by + cz + d =0
a <- cf2["TEMP"]
b <- cf2["PRESSURE"]
b
c <- -1
d <- cf2["(Intercept)"]

planes3d(a,b, c, d, alpha = 0.5, col = "Pink")


# Simple with interaction

fit3 = lm(QUALITY ~ PRESSURE*TEMP, data = prod)
cf3 = coef(fit3)
summary(fit3)
  
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

# Chapter 15
#Sign test

library(s20x)
library(readxl)
dird="C:\\Users\\stew9983\\OneDrive - University of Oklahoma\\MATH4773-5773\\DATA\\Excel\\"
#my function to read data
myread=function(xls){
  library(readxl)
  fl=paste0(dird,xls)
  read_xls(fl)
}
bacteria = myread("BACTERIA.xls")
head(bacteria)
library(BSDA)
SIGN.test(bacteria$ACTBAC, md=40,alternative = "greater")

act=bacteria$ACTBAC
library(ggplot2)
g=ggplot(bacteria, aes(x="Bcateria",y=ACTBAC,fill=I("Red")))+geom_boxplot() +geom_point() + geom_abline(intercept = 40, slope =0)
windows()
g
act - 40
sum(act-40>0)
sum(act-40<0)

pvalue = pbinom(3,size=10,prob=0.5)
pvalue

# or

1-pbinom(6,10,0.5)

# 15.3 robots

rob = myread("ROBOTS.xls")
wheels = rob$Wheels
wheels
library(s20x)
normcheck(wheels, shapiro.wilk = TRUE)

t.test(wheels,mu =3,alternative = "greater")
length(wheels)

#H_0: tau=3

indx=which(abs(wheels-3) > 0)
indx
whd=wheels[indx]-3
sum(whd>0)
sum(whd<0)
length(wheels[indx])
1-pbinom(8,17,0.5)
pbinom(8,17,0.5)


SIGN.test(wheels, md=3,alternative = "greater")

# 15.1 Starbucks

star=myread("STARBUCKS.xls")
head(star)

amount = star$AMOUNT

normcheck(amount,shapiro.wilk = TRUE)

amount-300
indx = which(amount-300 > 0 | amount -300 < 0) # or use abs()
indx
sum(amount[indx]-300>0)
sum(amount[indx]-300<0)

pbinom(1,5,0.5) #or
1-pbinom(3,5,0.5)

SIGN.test(amount, md = 300, alternative = "greater")

# techwrite

tec=myread("TECHWRITE.xls")
library(ggplot2)
g = ggplot(tec, aes(x=USER, y = RATING, fill = USER)) + 
             geom_boxplot() + geom_point()
g
head(tec)


ylm = lm(RATING ~ USER, data=tec)
normcheck(ylm, shapiro.wilk = TRUE)
levene.test(ylm)
t.test(RATING ~ USER, data=tec, mu=0)

rat=tec$RATING
rat
rnk = rank(rat)
sum(rnk[1:7])
sum(rnk[8:14])

tt=wilcox.test(RATING ~ USER, data = tec,alternative = "two.sided",mu=0)

qwilcox(p=c(0.05/2,1-0.05/2),7,7) +28
qsignrank(p=1-0.05/2,10)
wilcox.test(RATING ~ USER,data=tec,mu=0,paired=TRUE)

f = function(x) 4*(qt(1-0.05/2,4*x-4)/2.2)^2 - x # x: f:x->0
g = function(x) qt(1-0.05/2, 4*x-4)
windows()

# possble r values

r = seq(3,5,length=10000)
ff=f(r)

plot(r,ff)
index=sum(ifelse(ff<0,0,1))
index
ff[index:(index+1)] # sign change

(r[index]+r[index+1])/2 # interpolation

windows()
curve(f,xlim=c(2,10))
abline(h=0,v=c(3,4))
axis(1,3,3)




mynewt=function(x0,delta=0.00001,f){
  graphics.off()
  windows()
  fdash=function(x) (f(x+delta)-f(x))/delta
  
  d=1000
  i=0
  x=c()
  y=c()
  x[1]=x0
  y[1]=f(x[1])
  while(d > delta & i<10000){
    i=i+1
    x[i+1]=x[i]-f(x[i])/fdash(x[i])
    y[i+1]=f(x[i+1])
    d=abs(y[i+1])
  }
  
  curve(f(x),xlim=range(c(range(x),-range(x))),xaxt="n", main="Newton-Raphson Algorithm")
  points(x,y,col="Red",pch=19,cex=1.5)
  axis(1,x,round(x,2),las=2)
  abline(h=0,col="Red")
  
  segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=2)
  segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Pink")
  
  list(x=x,y=y)
}


mynewt(x0=3,delta=0.000001,f = f)



#nake a new class

mychiz=function(n,iter){
  z=rnorm(n*iter, mean=0, sd=1)
  mat= matrix(z^2,nr=n,nc=iter)
  zsq=apply(mat,2,sum)
  obj=list(zsq=zsq, mat=mat)
  class(obj)="myzsq"
  obj
}

out=mychiz(10,100)
class(out)

plot.myzsq = function(x, ...){
  layout(matrix(1:2,nr=1), width=1:2)
  hist(x$zsq, ...)
  barplot(x$mat)
}

methods(class="myzsq")

windows()
plot(out, col = "Red")



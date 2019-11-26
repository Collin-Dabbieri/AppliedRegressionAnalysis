# PURRR
library(purrr)

## map2
map2(1:10,11:20, ~.x+.y)

grd=expand.grid(1:10,11:20)
class(grd)
class(grd[1])
class(grd[,1])
zout=map2_dbl(grd[,1],grd[,2], ~dnorm(.x,mean=5)*dnorm(.y,mean=15),sd=100)
zout


#or using pmap

zout2 = pmap_dbl(list(grd[,1],grd[,2]),~dnorm(..1,mean=5)*dnorm(..2,mean=15) , sd=100)

zout-zout2

# pmap 3 components to list and multiple options
zoutm =pmap_dbl(list(grd[,1],grd[,2], grd[,1]),~dnorm(..1,mean=5)*dnorm(..2,mean=15)*dnorm(..3,mean=10) , c(sd=100,log=TRUE))

# map and split

head(mtcars)

cyl=split(mtcars, mtcars$cyl)
nms = rownames(cyl$`4`)
f = function(.x) {
  paste("This 4 cylinder car is called a ", .x)
}

map(nms, f)


# walk2
temp <- tempfile()
dir.create(temp)

gr <- split(mtcars, mtcars$gear)
gr
paths <- file.path(temp, paste0("gear-", names(gr), ".csv"))
obj=walk2(gr, paths, write.csv)

length(dir(temp))
dir(temp)
read.csv(paths[1])

# map read
obj2 = map(paths,~read.csv(.x))# fill in gap
obj2[1]

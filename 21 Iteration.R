#21 Iteration
library(tidyverse)
#21.2.1 Exercises
#1.Write for loops to:

#a)Compute the mean of every column in mtcars.
for (i in 1:ncol(mtcars)){
  mtcars_mean[,i]<-mean(mtcars[,i])
  
}
#b)Determine the type of each column in nycflights13::flights.
types<-names(nycflights13::flights)
for(i in 1:ncol(nycflights13::flights)){
  types[i]<-typeof(unlist(nycflights13::flights[,i]))
}
types

#c)Compute the number of unique values in each column of iris.
unique<-iris[1,]
for(i in 1:ncol(iris)){
  unique[i]<-length(unique(iris[,i]))
}
unique
#d)Generate 10 random normals for each of μ=−10, 0, 10, and 100.
# number to draw
n <- 10
# values of the mean
mu <- c(-10, 0, 10, 100)
normals <- vector("list", length(mu))
for (i in seq_along(normals)) {
  normals[[i]] <- rnorm(n, mean = mu[i])
}
normals

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

#Generate 10 random normals for each of μ=−10, 0, 10, and 100.
#Think about the output, sequence, and body before you start writing the loop.
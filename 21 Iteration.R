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


#21.3.5 Exercises

#1.Imagine you have a directory full of CSV files that you want to read in.
#You have their paths in a vector,
#files <- dir("data/", pattern = "\\.csv$", full.names = TRUE),
#and now want to read each one with read_csv(). Write the for loop that will load them into a single data frame.
files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)
df <- vector("list", length(files))
for(nm in names(files)){
  df[[nm]]<-read_csv(nm)
}
df


#2.aWhat happens if you use for (nm in names(x)) and x has no names? 
 a<-c(1,2,4,6)
for(nm in names(a)){
  print(a[[nm]])
}
 ##Nothing is printed

 #b.What if only some of the elements are named?

names(a)<-c("a","b")
a
for(nm in names(a)){
  print(a[[nm]])
}
##No-named columns are NA-values:Error in a[[nm]] : subscript out of bounds
#What if the names are not unique?
names(a)<-c("b","b", "c","d")
a
for(nm in names(a)){
  print(a[[nm]])
}
##They are printed the same, and the value corresponds to the first case of the name


#3.Write a function that prints the mean of each numeric column in a data frame, along with its name. For example, show_mean(iris) would print:

show_mean<-function(df){
  out<-list()
  for(nm in names(df)){
    if(is.numeric(df[[nm]]))
    out[[nm]] <- mean(df[[nm]])
  }
  out
}

show_mean(iris)
#> Sepal.Length: 5.84
#> Sepal.Width:  3.06
#> Petal.Length: 3.76
#> Petal.Width:  1.20


#4.What does this code do? How does it work?

trans <- list( 
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)

for (var in names(trans)) {
  ##if there is the name in trans,it applies it to the mtcars df
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}
library(tidyverse)
as_tibble(mtcars)
##It converts the disp column into liters and factors the am column
#1 Cubic Inch = 0.016387064 Liters
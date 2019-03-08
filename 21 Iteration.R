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


#21.4.1 Exercises
#1.Read the documentation for apply(). In the 2d case, what two for loops does it generalise?
?apply()
##It generalises for loops applied to the column, then elements in the resulting vector of a matrix.

#2.Adapt col_summary() so that it only applies to numeric columns You might want to start with an is_numeric() function that returns a logical vector that has a TRUE corresponding to each numeric column.
col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    if(is.numeric(df[[i]])){
      out[i] <- fun(df[[i]])
    }
  }
  out
}
library(tidyverse)
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10),
  e = c("a","b","c","d","e","f","g","h","i","j")
)

col_summary(df, mean)

#21.5.3 Exercises

#1.Write code that uses one of the map functions to:
  
#a.Compute the mean of every column in mtcars.

mtcars%>%map_dbl(mean)

#b.Determine the type of each column in nycflights13::flights.
flights<-nycflights13::flights
flights%>%map_chr(typeof)
#c.Compute the number of unique values in each column of iris.
iris%>%map_dbl(count_unique)

count_unique<-function(x){
  length(unique(x))
}
#d.Generate 10 random normals for each of μ=−10, 0, 10, and 100.
mu <- c(-10, 0, 10, 100)
mu %>% map(rnorm, n = 10)

#21.9.3 Exercises
#1.Implement your own version of every() using a for loop. Compare it with purrr::every(). What does purrr’s version do that your version doesn’t?
?every()
a<-list(c(19,1,2), letters, rnorm(19,3))
a%>%every(is.numeric)

        
manual_every<-function(fun, a){
  r<-c()
  for (i in 1:length(a)) {
      r<-c(r,(fun(a[[i]])))
  }
  if(sum(r)==length(a)){
    return(TRUE)
  }
  else{
    return(F)
  }
}
b<-list(c(1,2,3), c(2,4,6),1,"4")
manual_every(is.numeric, a=b)
?every()
##Additional arguments passed on to .f - my function has big O squared so it makes it less efficient


#2.Create an enhanced col_summary() that applies a summary function to every numeric column in a data frame.
col_summary_enhanced<-function(df, fun){
  map(df, fun)
}
df <- tibble(
  x = 1:3, 
  y = 3:1,
  z = c("a", "b", "c")
)
# OK
col_summary_enhanced(df, mean)
#3.A possible base R equivalent of col_summary() is:
col_sum3 <- function(df, f) {
  is_num <- sapply(df, is.numeric)
  df_num <- df[, is_num]
  
  sapply(df_num, f)
}
df <- tibble(
  x = 1:3, 
  y = 3:1,
  z = c("a", "b", "c")
)
# OK
col_sum3(df, mean)
# Has problems: don't always return numeric vector
col_sum3(df[1:2], mean)
col_sum3(df[1], mean)
col_sum3(df[0], mean)

##A problem of consistency, sapply() doesn't have specification for numeric vectors
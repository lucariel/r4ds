##19 Functions

#1.Why is TRUE not a parameter to rescale01()? What would happen if x contained a single missing value, and na.rm was FALSE?

##NA values spread across operations, only one that is not removed would cause the entire output to be NA

range(c(1,3,5,6, NA))

#2.In the second variant of rescale01(), infinite values are left unchanged. Rewrite rescale01() so that -Inf is mapped to 0, and Inf is mapped to 1.
rescale01 <- function(x) {
  if(x[!is.finite(x)] > 1){
    x[!is.finite(x)]<- 1
  }else{
    x[!is.finite(x)]<- 0
  }
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

x<-c(2,3,5,6, -Inf)

rescale01(x)


#3.Practice turning the following code snippets into functions. Think about what each function does. What would you call it? How many arguments does it need? Can you rewrite it to be more expressive or less duplicative?


x<-c(2,3,5,6, NA)
mean(is.na(x)) ##Gives NA proportion in input


na_prop<-function(x){
  mean(is.na(x)) 
  }
na_prop(x)

x / sum(x, na.rm = TRUE)##weight in total input %

porc_total<-function(x){
  x / sum(x, na.rm = TRUE)
  }


#https://en.wikipedia.org/wiki/Coefficient_of_variation
sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)


cv <- function(x){
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}
cv(x)


#5.Write both_na(), a function that takes two vectors of the same length and returns the number of positions that have an NA in both vectors.
x<-c(2,NA,5,6, NA,1,3,NA, NA,NA,2,NA)
y<-c(2,2,NA,6, NA,2,1,NA, NA,NA,1,1)
length(y)
both_na<-function(x,y){
  y1<-which(is.na(y))
  x1<-which(is.na(x))
  length(y1)=length(x1)
  r<-c(y1==x1)
  return(sum(r, na.rm = T))

}
both_na(x,y)
#6.What do the following functions do? Why are they useful even though they are so short?

is_directory <- function(x) file.info(x)$isdir  #Determines if an object is a directory (return TRUE)or not (return False)
is_readable <- function(x) file.access(x, 4) == 0 #Determines if an object is a readable (return TRUE)or not (return False)

##They provide help managin files and directories 






#19.3.1 Exercises


#1.Read the source code for each of the following three functions, puzzle out what they do, and then brainstorm better names.

has_prefix <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}


remove_last <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}

n_rep <- function(x, y) {
  rep(y, length.out = length(x))
}

#2.Take a function that you’ve written recently and spend 5 minutes brainstorming a better name for it and its arguments.

#3.Compare and contrast rnorm() and MASS::mvrnorm(). How could you make them more consistent?

?rnorm()
m<-rnorm(5, mean = 0, sd = 1)
#return a vector
library(MASS)
n<-MASS::mvrnorm(n=5, mu = 0, Sigma = 1)
##Return a matrix
class(m)
class(n)
###Besides they take arguments which refer to similar concepts and yet they have different name, When possible they should have the same.

#4.Make a case for why norm_r(), norm_d() etc would be better than rnorm(), dnorm(). Make a case for the opposite.
##norm_r and norm_d would be better because they belong to the same family of functions and we can take advantage of R's autocmplete
##On the other and, rnorm() and dnorm() are shorter


#19.4.4 Exercises

#1. What’s the difference between if and ifelse()? Carefully read the help and construct three examples that illustrate the key differences.

?ifelse() #Is a built function. Requires alternatives for both true and false. The return value is always given by the function.
?`if` #Is a base function. Requires only the true alternative and finishes without returning at false


#2.Write a greeting function that says “good morning”, “good afternoon”, or “good evening”, depending on the time of day. (Hint: use a time argument that defaults to lubridate::now(). That will make it easier to test your function.)
library(lubridate)

greeter<- function(){
  if(hour(now())<=13){
    return("good morning")
  }
  ifelse(hour(now())<=19,return("good afternoon"), return("good evening"))
  }
greeter()
#3.Implement a fizzbuzz function. It takes a single number as input. If the number is divisible by three, it returns “fizz”. If it’s divisible by five it returns “buzz”. If it’s divisible by three and five, it returns “fizzbuzz”. Otherwise, it returns the number. Make sure you first write working code before you create the function.

fizzbuzz<-function(x){
  if(x%%3==0 && x%%5==0){
    return("fizzbuzz")
  }
  if(x%%3==0 && x%%5!=0){
    return("fizz")
  }
  
  if(x%%3!=0 && x%%5==0){
    return("buzz")
  }
}
#4.How could you use cut() to simplify this set of nested if-else statements?

how_is_weater<-function(temp){
  cut(temp, c(-Inf, 0, 10, 20, 30, Inf),
      right = TRUE,
      labels = c("freezing", "cold", "cool", "warm", "hot")
  )
}
#5.What happens if you use switch() with numeric values?

?switch()
##If the value evaluated is a number, that item of the list is returned.
###(DOC: If the value of EXPR is not a character string it is coerced to integer
### If the integer is between 1 and nargs()-1 then the corresponding element of ... is evaluated and the result returned)

#6.What does this switch() call do? What happens if x is “e”?
x = "e"
switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd",
       "s"  ##Unnamed: returned because of no-match scenario
)
# In the case of no match, if there is a unnamed element of ... its value is returned.
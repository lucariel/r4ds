##STRINGS##
library(tidyverse)
library(stringr)

x <- c("amigo", "volar", "violin")
x
writeLines(x)
str_length(x)
str_c(x[1], x[2], sep = ",")


y <- c("Apple", "Banana", "Pear")
str_sub(y, 1,3)
str_sub(y, -3,-1)
str_c(str_sub(y, 1,3),str_sub(y, -3,-1) )
##14.2.5 Exercises

##1.In code that doesn’t use stringr, you’ll often see paste() and paste0(). What’s the difference between the two functions? What stringr function are they equivalent to? How do the functions differ in their handling of NA?
?paste()
?paste0()
c0<-paste0(x, y)
s0<-paste(x, y)
#The difference between paste() and paste0() is that the argument sep by default is ” ” (paste) and “” (paste0).


##2.In your own words, describe the difference between the sep and collapse arguments to str_c().
?str_c()
length(str_c(x, y, collapse = ","))
length(str_c(x, y, sep = ","))

#Collapse returns a vector of value 1, sep keeps size of vector of the combined strings


##3.Use str_length() and str_sub() to extract the middle character from a string. What will you do if the string has an even number of characters?
s<- "Aminoacido"
str_sub(s, str_length(s)/2,str_length(s)/2+1)


##4.What does str_wrap() do? When might you want to use it?

?str_wrap()
#implements the Knuth-Plass paragraph wrapping algorithm. Which format paragraphs so they are well writen.
#It might be usefull to publish, or to read better a text when it is imported.

##5.What does str_trim() do? What’s the opposite of str_trim()?

?str_trim()
#str_trim() removes whitespace from start and end of string;

g<- "                 this a house       "
h<-str_trim(g)
writeLines(h)
writeLines(g)

#and the oposite would be 
str_trim(str_pad(h, 30, side=c("both"), pad = " "))
#to add whitespace (or other). The difference is that they are not
#perfect oposites since you could pad it with other character than whitespaces and it would be trimmed
str_trim(str_pad(h, 30, side=c("both"), pad = "."))


##6.Write a function that turns (e.g.) a vector c("a", "b", "c") into the string a, b, and c. Think carefully about what it should do if given a vector of length 0, 1, or 2.



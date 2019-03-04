#20 Vectors

#20.3.5 Exercises
#1.Describe the difference between is.finite(x) and !is.infinite(x).
is.finite(NA) #FALSE
!is.infinite(NA) # TRUE
## The first one returns True only if the argument is finite, which would give False in
## cases of NA or NaN, the second returns True only for non-infinite. This difference
## arises because there more than 1 special value.

#2.Read the source code for dplyr::near() (Hint: to see the source code, drop the ()). How does it work?

dplyr::near
##If the difference between x and y is less than the system's double approx (.Machine$double.eps^0.5) method
##returns true.

#3.A logical vector can take 3 possible values. How many possible values can an integer vector take? How many possible values can a double take? Use google to do some research.
##For integers vectors, R uses a 32-bit representation. This means that it can represent up to 232 different values with integers. One of these values is set aside for NA_integer_. From the help for integer
.Machine$integer.max
##All R platforms are required to work with values conforming to the IEC 60559 (also known as IEEE 754) standard. This basically works with a precision of 53 bits, and represents to that precision a range of absolute values from about 2e-308 to 2e+308. It also has special values NaN (many of them), plus and minus infinity and plus and minus zero (although R acts as if these are the same). There are also denormal(ized) (or subnormal) numbers with absolute values above or below the range given above but represented to less precision.


#4.Brainstorm at least four functions that allow you to convert a double to an integer. How do they differ? Be precise.
##4 alternatives using round, floor, ceiling and trunc
##round numbers to the nearest possibility
a<-34.3
double_to_int1<-function(x){
  x<-as.integer(round(a))
  x
}
double_to_int1(a)
a<-34.3
##rounds to the nearest integer that’s smaller than x
double_to_int2<-function(x){
  x<-as.integer(floor(a))
  x
  }
double_to_int2(a)
a<-34.3

## rounds to the nearest integer that’s larger than x.
double_to_int3<-function(x){
  x<-as.integer(ceiling(a))
  x
  }

double_to_int3(a)1
a<-34.3
##rounds to the nearest integer in the direction of 0
double_to_int4<-function(x){
  x<-as.integer(trunc(a))
  x
  }

double_to_int4(a)
#5.What functions from the readr package allow you to turn a string into logical, integer, and double vector?
##parse_logical(),parse_integer(), parse_number()



#20.4.6 Exercises

##Atomic vectors are linear vectors of a single primitive type

#1.What does mean(is.na(x)) tell you about a vector x? 
## the proportion of NA values in a vector
#What about sum(!is.finite(x))?
## the quantity of non-numeric values

#2.Carefully read the documentation of is.vector(). What does it actually test for? 
##is.vector returns TRUE if x is a vector of the specified mode having no attributes  
##other than names. It returns FALSE otherwise. Making list objects, vectors Unlike is.atomic()

##The atomic modes are "logical", "integer", "numeric" (synonym "double"),
##"complex", "character" and "raw".


#Why does is.atomic() not agree with the definition of atomic vectors above?
?is.atomic()
##is.atomic is true for the atomic types ("logical", "integer", "numeric", 
##"complex", "character" and "raw") and NULL.
##The definitions dont agree because is.atomic includes NULL as atomic types

#3.Compare and contrast setNames() with purrr::set_names().
##The main difference between both is that setNames names objects, and set_names() names vectors

?setNames()
library(purrr)
?set_names()
#4.Create functions that take a vector as input and returns:
  #The last value. Should you use [ or [[?
lastValue<-function(x){
  if(is.vector(x)){
    return(x[length(x)])
  }
}

  #The elements at even numbered positions.
evenPositions<-function(x){
  e<-c()
  for(i in seq(from = 2, to =length(x), by = 2)){
    e<-c(e, x[i])
  }
  e
}
  #Every element except the last value.
allButLast<-function(x){
  x[-length(x)]
}
  #Only even numbers (and no missing values).
                                      
evenNumbers<-function(x){
  x[!as.logical(x%%2)]
}

#5.Why is x[-which(x > 0)] not the same as x[x <= 0]?
##The first option includes NaN. and which() returns indices, while x<=0 returns logical vector


#6.What happens when you subset with a positive integer that’s bigger than the length of the vector? 
x[100] ##Returns NA
#What happens when you subset with a name that doesn’t exist? 
x<-c(a=1, b=2)
x["j"]


#20.5.4 Exercises

#1.Draw the following lists as nested sets:
  
list(a, b, list(c, d), list(e, f))

#-----------#
#[[A]]-[[B]]
#-[[C],[D]]-#
#-[[E],[F]]-#
list(list(list(list(list(list(a))))))
#[[[[[[[a]]]]]]]


#2.What happens if you subset a tibble as if you’re subsetting a list? What are the key differences between a list and a tibble?
a<-as.tibble(cbind(c(1,2,3), c(1,2,4)))
a$V1
a[[1]][[2]]
##The subsetting of the tibble works fine. The difference is that a tibble is a list but a list is no necessary to be a tibble
########The difference between a tibble and a list is that all the elements of a data frame must be vectors with the same length


#20.7.4 Exercises

#1.aWhat does hms::hms(3600) return? 
hms::hms(3600) ##prints 01:00:00
#1.b How does it print? ##Is the number of hours in 3600 seconds
#1.c What primitive type is the augmented vector built on top of? ## numeric
#1.d What attributes does it use?
attributes(hms::hms(3600))


#2.Try and make a tibble that has columns with different lengths. What happens?
as.tibble(c(1,2,3), c(1,2))
##Warning message:
##In as.data.frame.numeric(value, stringsAsFactors = FALSE, ...) :
##  'row.names' is not a character vector of length 3 -- omitting it. Will be an error!#

##Vectors that are not compatible are omitted

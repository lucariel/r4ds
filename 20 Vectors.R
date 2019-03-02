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


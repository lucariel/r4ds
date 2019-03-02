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

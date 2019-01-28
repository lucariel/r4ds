#10 Tibbles
library(tidyverse)

##To convert a data frame into a tibble
as_tibble(iris)

##Or to create a tibble from scratch

tibble(
  x = 1:5,
  y = 1,
  z = x^2+y
)
##One diference so far: a tibble never convert types: eg:string to factor.
##A tibble can have non-syntactic names one needs to use `` [these are also useful in other packeges such as ggplot2,dplyr,tydr].

tb<- tibble(
  `:)`="smile",
   ` `="space",
   `2000`="number"
)

##Another way to creat a tibble is with
tribble(
  ~x,~y,~z,
  "a",2,3.6,
  "b",1,8.5
)


##Key differences: printing and subsetting.

##As for printing:

####Tibbles have a refined print method that shows only the first 10 rows.
####each column reports its type.

###print(n = k{any number of rows}, width = Inf {all collumns})
###View() to see the entire dataset

df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
##these are synonyms
df[["x"]]
df$x
df[[1]]
##Using the pipe
df%>%.$x
df%>%.[["x"]]
df%>%.[[1]]
#### they never do partial matching
#### and they will generate a warning if the column you are trying to access does not exist

##Functions which are not suitable for tibbles
class(as.data.frame(df))
class(df)


#10.5 Exercises
#1.How can you tell if an object is a tibble? (Hint: try printing mtcars, which is a regular data frame).

mtcars  #Displays every row, it doesnt say de type 
as.tibble(mtcars) #10 rows, says the type

#2.Compare and contrast the following operations on a data.frame and equivalent tibble. What is different? Why might the default data frame behaviours cause you frustration?

df <- data.frame(abc = 1, xyz = "a")
df$x #in df, parctial matching
df[, "xyz"]  #converts to factor, in df
df[, c("abc", "xyz")] #printing differs, it's cleaner and clearer in tibble


df_t<-as.tibble(df)
df_t$x
df_t[, "xyz"]
df_t[, c("abc", "xyz")]
#3.If you have the name of a variable stored in an object, e.g. var <- "mpg", how can you extract the reference variable from a tibble?
df_t2<-tibble(
  mpg="this is inside the tibble"
)

df_t2[["mpg"]]
df_t2$mpg
#or
df_t2%>%.$mpg
df_t2%>%.[["mpg"]]
#4.Practice referring to non-syntactic names in the following data frame by:
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
#Extracting the variable called 1.
annoying[[1]]

#Plotting a scatterplot of 1 vs 2.
library(ggplot2)

annoying%>%
  ggplot(mapping = aes(x =annoying[[1]], y = annoying[[2]] ))+
  geom_point()


#Creating a new column called 3 which is 2 divided by 1.

annoying<-annoying%>%
  mutate(
    `3`=annoying[[2]]/annoying[[1]]
  )
#Renaming the columns to one, two and three.
names(annoying)<-c("one", "two", "three")

#5.What does tibble::enframe() do? When might you use it?

?enframe()
##To create new data or to construct the df and then to name them
d<-enframe(1:3)
names(d)=c("v1", "v2")


#6.What option controls how many additional column names are printed at the footer of a tibble?
print(n = k{any number of rows}, width = Inf {all collumns})



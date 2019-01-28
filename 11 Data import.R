#11 Data import
library(tidyverse)


##There are several functions to read files but all have similar syntax so let's focus in:

heights <- read_csv("data/heights.csv")


##read_csv uses the first line of data as the columns, as convention but to customize
## you can use the parameter skip = n, n is to skip the first n lines. 
read_csv("The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3", skip = 2)

##or the parameter comment="#
##so R skips commented. Note that you can especify the char of the comment, applicable for working with other formats

read_csv("# A comment I want to skip
  x,y,z
  1,2,3", comment = "#")
##Letssay now that there are not colnames in the first space

read_csv("1,2,3\n4,5,6", col_names = FALSE)
##col_names  parameter can be filled with a vector with the desired names.

read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))


##When you have NA values identified as any specific character you can use the parameter na=""

read_csv("a,b,c\n1,2,.", na = ".")


#11.2.1 Compared to base R- base R's function is read.csv instead of read_csv

#11.2.2 Exercises
#1.What function would you use to read a file where fields were separated with
#“|”?
  
read_delim()

#2.Apart from file, skip, and comment, what other arguments do read_csv() and read_tsv() have in common?
?read_csv()
?read_tsv()

##all

#3.What are the most important arguments to read_fwf()?
?read_fwf()
##Besides the file, I would say widths Acording to doc:
###Unfortunately, it's painful to parse because you need to describe the length of every field

#4.Sometimes strings in a CSV file contain commas. To prevent them from causing problems they need to be surrounded by a quoting character, like " or '. By convention, read_csv() assumes that the quoting character will be ", and if you want to change it you’ll need to use read_delim() instead. What arguments do you need to specify to read the following text into a data frame?
read_delim("x,y\n1,'a,b'", quote = "\'", delim = ",")
?read_delim()

#5.Identify what is wrong with each of the following inline CSV files. What happens when you run the code?


read_csv("a,b\n1,2,3\n4,5,6") 
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b\n\"1")
##For the above, the number of rows and columns is not compatible
read_csv("a,b\n1,2\na,b")##This doesnt have a problem. 
read_csv("a;b\n1;3")#Syntax error, delim = ";"



#11.3 Parsing a vector {parse_*() functions}
##parse_*(c(), na = "*")

#Parse Numbers
parse_double("1.23")
parse_double("1,23", locale = locale(decimal_mark = ","))

parse_number("It cost $123.456.789", locale = locale(grouping_mark = "."))

#11.3.2 Strings

## underlying representation of a string in UTF-8 using:
charToRaw("Hadley")
##You can change the encoding to others such as Latin1, Latin2 and ASCII
x1 <- "El Ni\xf1o was particularly bad this year"
parse_character(x1, locale = locale(encoding = "Latin1"))
##To find the correct encoding: it's usually a metadata info, in case it's not there
guess_encoding(charToRaw(x1))
parse_character(x1, locale = locale(encoding = "ISO-8859-1"))


#11.3.3 Factors
##parse_factor(c(vector, to, parse),level=c(a1, a2, a3))
fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)

#11.3.4 Dates, date-times, and times
#parse_datetime() #expects an ISO8601 date-time

parse_datetime(20101210) 
#parse_date() expects a four digit year, a - or /, the month, a - or /, then the day:
parse_date(20101010)#error
parse_date("2010-10-10") #OK

# 
# 
# Year
# %Y (4 digits). 
# %y (2 digits); 00-69 -> 2000-2069, 70-99 -> 1970-1999. 
# Month
# %m (2 digits). 
# %b (abbreviated name, like “Jan”). 
# %B (full name, “January”). 
# Day
# %d (2 digits). 
# %e (optional leading space). 
# Time
# %H 0-23 hour. 
# %I 0-12, must be used with %p. 
# %p AM/PM indicator. 
# %M minutes. 
# %S integer seconds. 
# %OS real seconds. 
# %Z Time zone (as name, e.g. America/Chicago). Beware of abbreviations: if you’re American, note that “EST” is a Canadian time zone that does not have daylight savings time. It is not Eastern Standard Time! We’ll come back to this time zones. 
# %z (as offset from UTC, e.g. +0800). 
# Non-digits
# %. skips one non-digit character. 
# %* skips any number of non-digits. 


parse_date("01/02/15", "%m/%d/%y")
#> [1] "2015-01-02"
parse_date("01/02/15", "%d/%m/%y")
#> [1] "2015-02-01"
parse_date("01/02/15", "%y/%m/%d")
#> [1] "2001-02-15"
##Notice that the same String returns different dates acording to the second parameter


#11.3.5 Exercises
#1.What are the most important arguments to locale()?

?locale() ##It's too broad question, it depends ultimalty in the parsing data, where in the world you are and how different are the necessary parameters from the default


#2.What happens if you try and set decimal_mark and grouping_mark to the same character? What happens to the default value of grouping_mark when you set decimal_mark to “,”? What happens to the default value of decimal_mark when you set the grouping_mark to “.”?


parse_number("It cost $123.456,789", locale = locale(decimal_mark = "."))
##Error, grouping and decimal mark must be different from each other
##Each mark gets interchanged when especify "." or ","

#3.I didn’t discuss the date_format and time_format options to locale(). What do they do? Construct an example that shows when they might be useful.

?locale()
parse_date("01/02/15", locale = locale(date_format = "%m/%d/%y"))


#4.If you live outside the US, create a new locale object that encapsulates the settings for the types of file you read most commonly.

es<-locale("es", decimal_mark = ",")
parse_number("It cost $123.456,789", locale =es)


#5.What’s the difference between read_csv() and read_csv2()?
?read_csv()
?read_csv2()
##The difference is between the default parameters, which are the most used


#7.Generate the correct format string to parse each of the following dates and times:
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"



parse_date(d1, "%B %d, %Y")
parse_date(d2, "%Y-%b-%d")
parse_date(d3, "%d-%b-%Y")
parse_date(d4, "%B %d (%Y)")
parse_date(d5, "%m/%d/%y")
parse_time(t1, "%H%M")
parse_time(t2)

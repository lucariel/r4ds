#16 Dates and times

library(tidyverse)
library(lubridate)
library(nycflights13)

class(today())#"Date"
class(now())#"POSIXct" "POSIXt" 

#16.2.4 Ex
#1.What happens if you parse a string that contains invalid dates?
test1<-ymd(c("2010-10-10", "bananas"))
class(test1)
##The invalid data get turned into NA and the parsing still return a datetime.


#2.What does the tzone argument to today() do? Why is it important?
?today()

##Depends on the hour of day, it would give wrong date since:
####tzone defaults to the system time zone set on your computer.
##and you migth be working with data from/for another timezone

#3.Use the appropriate lubridate function to parse each of the following dates:
d1 <- "January 1, 2010"
d1p<-mdy(d1)#[1] "2010-01-01"

d2 <- "2015-Mar-07"
d2p<- ymd(d2)#"2015-03-07"

d3 <- "06-Jun-2017"#"2017-06-06"
d3p<- dmy(d3)

d4 <- c("August 19 (2015)", "July 1 (2015)")
d4p <- c(mdy(d4[1]),mdy(d4[2])#[1] "2015-08-19" "2015-07-01"



d5 <- "12/30/14" # Dec 30, 2014
d5p<- mdy(d5)#[1] "2014-12-30"


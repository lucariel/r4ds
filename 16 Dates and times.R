#16 Dates and times

library(tidyverse)
library(lubridate)
library(nycflights13)

###############################################
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

head(flights_dt)
##########################################################

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
d4p <- c(mdy(d4[1]),mdy(d4[2]))#[1] "2015-08-19" "2015-07-01"



d5 <- "12/30/14" # Dec 30, 2014
d5p<- mdy(d5)#[1] "2014-12-30"


#16.3.4 Exercises

#1.How does the distribution of flight times within a day change over the course of the year?
head(month(flights_dt$dep_time))


flights_2<-flights_dt%>%group_by(month(flights_dt$dep_time, label = T))%>%
  summarise(airtime_avg = mean(air_time, na.rm = T))

colnames(flights_2) <- c("Month", "avg_airtime")


month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

flights_2$Month<-factor(flights_2$Month, levels = month_levels)
flights_2%>%ggplot(aes(x = Month , y = avg_airtime))+geom_col()



#2.Compare dep_time, sched_dep_time and dep_delay. Are they consistent? Explain your findings.
flights_3<-flights_dt%>%
  mutate(calc_deldelay = abs(as.numeric((dep_time- sched_dep_time)/60)),
         dep_delay = abs(dep_delay),
         diference = calc_deldelay-dep_delay
         )%>%select(diference,origin )%>%filter(diference >1)
##There seems to  be a cause of registration in some origin airports

flights_4<-flights_3%>%group_by(origin)%>%summarise(n = n())
flights_4$origin #"EWR" "JFK" "LGA" are the airports with deferences



#3.Compare air_time with the duration between the departure and arrival. Explain your findings. (Hint: consider the location of the airport.)
flights4 <- flights_dt%>%
  mutate(calc_airtime = as.numeric(arr_time-dep_time))%>%
  select(calc_airtime, air_time, origin, dest)%>%group_by(origin)%>%summarise(n = n())

#Is the same situation as before, it's related to the EWR JFK and LGA airports

#4.How does the average delay time change over the course of a day? Should you use `dep_time` or `sched_dep_time`? Why?
flights_dt %>% 
  mutate(sched_dep_hour = hour(sched_dep_time))%>%
  group_by(sched_dep_hour)%>%
  summarise(dep_delay = mean(dep_delay))%>%
  ggplot(aes(x = sched_dep_hour, y = dep_delay))+
  geom_line()


#5.On what day of the week should you leave if you want to minimise the chance of a delay?
flights_weeks<-flights_dt %>% 
  mutate(sched_dep_week = weekdays(sched_dep_time))%>%
  group_by(sched_dep_week)


View(weekdays(flights_dt$sched_dep_time))
weekdays_levels <- c(
  "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", 
  "Sunday"
)

flights_weeks$sched_dep_week<-factor(flights_weeks$sched_dep_week, levels = weekdays_levels)
flights_weeks%>%
  summarise(dep_delay = mean(dep_delay))%>%
  arrange(sched_dep_week)%>%
  ggplot(aes(x = sched_dep_week, y = dep_delay))+
  geom_col()
##You should go on saturday
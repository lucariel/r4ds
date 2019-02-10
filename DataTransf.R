#Data Transformtion
library(nycflights13)
library(tidyverse)
library(funModeling)
library(Lahman)

View(flights)
flights


#5.2 Filter rows with filter()
filter(flights, month == 1, day == 1)
(jan2 <-filter(flights, month == 1, day == 1))
(jan1 <-filter(flights, month == 1, day == 1))
identical(jan1,jan2)
(dec25<-filter(flights, month ==12, day ==25))
jan1


#5.2.1 Comparisons
filter(flights, month>2)
filter(flights, month == 1)

nov_dec2<-filter(flights, month == 11 | month == 12)
(nov_dec<-filter(flights, month %in% c(11,12)))
identical(nov_dec, nov_dec2)
identical(
  filter(flights, !(arr_delay > 120 | dep_delay > 120)),
  filter(flights, arr_delay <= 120, dep_delay <= 120))


#Find all flights that
#Had an arrival delay of two or more hours

(two_hr_late <-filter(flights, arr_delay>2 ))

#Flew to Houston (IAH or HOU)

(to_Houston <- filter(flights, dest == "HOU" | dest =="IAH"))
View(to_Houston)

#Were operated by United, American, or Delta
(operated<- filter(flights, carrier == "UA" | carrier =="AA" | carrier == "DL"))
View(operated)

#Departed in summer (July, August, and September)
summer_flights <- filter(flights, month %in% c(7,8,9))
View(summer_flights)

#Arrived more than two hours late, but didn’t leave late
flights
arr_late_leave_ok <- filter(flights, arr_delay>2, dep_delay<=0)
arr_late_leave_ok

#Were delayed by at least an hour, but made up over 30 minutes in flight
(least_hr_30_min <- filter(flights, dep_time-arr_time<30, dep_delay>1))

#Departed between midnight and 6am (inclusive)
between_midnight_and_6am<- filter(flights, dep_time<=600)
between_midnight_and_6am

#Another useful dplyr filtering helper is between().
#What does it do? Can you use it to simplify the code needed to answer
#the previous challenges?
?between()
summer_flights_2<-filter(flights, between(month, 7,9))
identical(summer_flights_2, summer_flights)

#How many flights have a missing dep_time?
#What other variables are missing? What might these rows represent?
na_analysis<-filter(df_status(flights),q_na>0)
View(flights)
# 
# Why is NA ^ 0 not missing? Why is NA | TRUE not missing? 
#   Why is FALSE & NA not missing? Can you figure out the general rule? 
#   (NA * 0 is a tricky counterexample!)
# 
# 
# 
# 
NA^0
#5.3 Arrange rows with arrange()
flights %>% 
  arrange(desc(is.na(dep_time)),
          desc(is.na(dep_delay)),
          desc(is.na(arr_time)), 
          desc(is.na(arr_delay)),
          desc(is.na(tailnum)),
          desc(is.na(air_time)))
# 

# Sort flights to find the most delayed flights. Find the flights that left earliest.
arrange(flights, desc(dep_delay), desc(arr_delay))

# Sort flights to find the fastest flights.
shortest<-arrange(flights, desc(-distance))
shortest$distance
longest<-arrange(flights, desc(distance))
longest$distance


#5.4 Select columns with select()

select(flights, year, month, day)


#5.4.1 Exercises

#a)Brainstorm as many ways as possible to select dep_time,
# dep_delay, arr_time, and arr_delay from flights.

op1<-select(flights, dep_time, dep_delay, arr_time, arr_delay)

op2<-flights %>%
  select(
    dep_time,
    dep_delay,
    arr_time,
    arr_delay
  )

identical(op1, op2) #true

op3<- flights%>%
  select(starts_with("dep"), starts_with("arr"))

identical(op1, op3) #true

op4<- flights %>%
  select(ends_with("time"), ends_with("delay"))

identical(op1, op4) #false






#b) What happens if you include 
#the name of a variable multiple times in a select() call?
select(flights, year)
select(flights, year, year) #R just ignores the second time

#3.What does the one_of() function do?
# Why might it be helpful in conjunction with this vector?
?one_of()
vars <- c("year", "month", "day", "dep_delay", "arr_delay")

select(flights, one_of(vars))
select(flights, one_of("j"))
#asks whether the variable name is one of the names listed in the character vector


#Does the result of running the following code surprise you? How do the select helpers deal with case by default? How can you change that default?

select(flights, contains("TIME", ignore.case = FALSE)) #It's surprising that R istn not case sensitve

?select()
?contains()

#5.5 Add new variables with mutate()

flights_slm<-select(flights,
                    year : day,
                    ends_with("delay"),
                    distance,
                    air_time
                    )

flights_slm

flights_slm_2<-mutate(flights_slm,
       gain = dep_delay-arr_delay,
       speed = distance/air_time*60
       
       )



transmute(flights_slm,
          gain = dep_delay-arr_delay,
          speed = distance/air_time*60
          )

identical(flights_slm, flights_slm_2)
mutate(flights_slm,
       hours = air_time/60,
       gain_per_hour = gain/hours
       )

colnames(flights)

mutate(flights_slm,
       proportion =((dep_delay+arr_delay)/ air_time)*100
       )

# #5.5.2 Exercises
# 
#1. Currently dep_time and sched_dep_time are
# convenient to look at, but hard to compute with because 
# they’re not really continuous numbers.
# Convert them to a more convenient representation 
# of number of minutes since midnight.
#
colnames(flights)
f2<-flights %>%
  mutate(dep_time_min =  as.integer(dep_time/100)*60 + (dep_time/100 - as.integer(dep_time/100))*100,
         sched_dep_time_min =  as.integer(sched_dep_time/100)*60 + (sched_dep_time/100 - as.integer(sched_dep_time/100))*100

         )
f2%>%
  transmute(delay_calc = dep_time_min-sched_dep_time_min)
#Compare air_time with arr_time - dep_time. What do you expect to see? What do you see? What do you need to do to fix it?

flights %>%
  transmute(air_time,
            arr_time-dep_time)

#I would expect that both are the same. But they are different
#dep_time&arr_time format HHMM or HMM
#air_time:Amount of time spent in the air, in minutes
#meaning that one must convert to minutes past midnight dep_time&arr_time 

f3<- flights %>%
  transmute(
    dep_time_min =  as.integer(dep_time/100)*60 + (dep_time/100 - as.integer(dep_time/100))*100,
    
    arr_time_min = as.integer(arr_time/100)*60 + (arr_time/100 - as.integer(arr_time/100))*100
    
  )
flights %>%
  select(arr_time, dep_time)
f3%>%
  select(arr_time_min, dep_time_min)
f3%>%
  transmute(
    air_time =arr_time_min-dep_time_min 
    
    
  )
#What does 1:3 + 1:10 return? Why?
##To each element of the longer array, sum each elemnt of the shorter, this means
##that they must be divisible. And that is why 1:3+1:10 gives warning
 1:3 + 1:10
 #Warning message:
 # In 1:3 + 1:10 :
 #   longer object length is not a multiple of shorter object length
 
  1:3 + 1:12
 #[1]  2  4  6  5  7  9  8 10 12 11 13 15
  1:12
 #[1]  1  2  3  4  5  6  7  8  9 10 11 12

  
  
#5.6 Grouped summaries with summarise()
  summarise(flights, delay = mean(dep_delay, na.rm = T))
?group_by()
  by_day<- group_by(flights, year, month , day)
  summarise(by_day, delay = mean(dep_delay, na.rm = T))
  
#5.6.1 Combining multiple operations with the pipe

  ?geom_smooth()
  #Imagine that we want to explore the relationship between the distance and average delay for each location
  by_dest <- group_by(flights, dest)
  delay <- summarise(by_dest,
                     count = n(),
                     dist = mean(distance, na.rm = TRUE),
                     delay = mean(arr_delay, na.rm = TRUE)
  )
  delay <- filter(delay, count > 20, dest != "HNL")
  
  
  ggplot(data = delay, mapping = aes(x=dist, y=delay))+
    geom_point(aes(size =count), alpha = 1/3)+
    geom_smooth(se = FALSE)
  
  colnames(flights)
  by_airport <- group_by(flights,origin)
  by_airport<-summarise(by_airport,
            count = n()
            )
  
  
  
  by_destination<-group_by(flights, dest)
  by_destination2<-summarise(by_destination,
            count = n()
            )
  ggplot(data = by_destination2, mapping = aes(x = count))+
    geom_freqpoly(bin = 100)
  
  ?geom_bar()
  by_destination
  ggplot(data = by_airport)+
    geom_col(mapping = aes(x = origin, y = count))
    #But with pipe operator %>%
delays<-flights%>%
  group_by(dest)%>%
  summarise(
    by_dest,
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  )%>%
  filter(delay, count>20, dest != "HNL")
#5.6.2 Missing values

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))
#5.6.3 Counts
delays<-not_cancelled%>%
  group_by(tailnum)%>%
  summarise(
    delay = mean(arr_delay)
  )
ggplot(data =delays, mapping = aes(x = delay))+
  geom_freqpoly(binwidth = 10)
#Adding the count
delays<-not_cancelled%>%
  group_by(tailnum)%>%
  summarise(
    delay = mean(arr_delay),
    n = n()
  )
ggplot(data = delays, mapping = aes(x = n, y = delay))+
  geom_point(alpha = 1/10)


#Filtering out groups with smallest number of observations
delays %>%
  filter(n>25)%>%
  ggplot(mapping = aes(x = n , y = delay))+
  geom_point(alpha = 1/10)

####Analysis of the Lahman Data####
batting <- as_tibble(Lahman::Batting)
colnames(batting)

batters<-batting%>%
  group_by(playerID)%>%
  summarise(
    
    ba = sum(H, na.rm = T)/sum(AB, na.rm = T),
    ab = sum(AB, na.rm = T)
    
  )
batters%>%
  filter(ab>100)%>%
  ggplot(mapping = aes(x = ab, y = ba))+
  geom_point()+
  geom_smooth(se = FALSE)

 
# 5.6.4 Useful summary functions
not_cancelled%>%
  group_by(year, month, day)%>%
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay>0])
    
  )
not_cancelled%>%
  group_by(dest)%>%
  summarise(distance_sd = sd(distance), distance_iqr = IQR(distance))%>%
  arrange(desc(distance_iqr, distance_sd))

not_cancelled%>%
  group_by(year, month, day)%>%
  summarise(
    first = min(dep_time),
    last = max (dep_time)
    
  )
length(not_cancelled$year)

#Filtering gives you all variables, with each observation in a separate row:
not_cancelled%>%
  group_by(year, month, day)%>%
  mutate(r = min_rank(desc(dep_time)))%>%
  filter(r %in% range(r))

#To count the number of distinct (unique) values, use n_distinct(x).
not_cancelled%>%
  group_by(dest)%>%
  summarise(carriers = n_distinct(carrier))%>%
  arrange(desc(carriers))
not_cancelled%>%
  count(dest)%>%
  filter(n>200)
?count()
#5.6.7 Exercises


# 
# 1)Brainstorm at least 5 different ways to assess the typical delay characteristics of a group of flights. Consider the following scenarios:
#   
#   1.a)A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time.

not_cancelled

(delays<-not_cancelled%>%
  group_by(flight)%>%
  summarise(
    delay = median(dep_delay)
  )%>%
  arrange(-desc(delay)))%>%
  filter(
    delay == -30 | delay == 30
  )
#The problem with this solution is that I'm taking the median, that means that I'm calculating
#the flights whichs median es greater or equal to the number

#A flight is always 10 minutes late.

(delays_10min <- not_cancelled %>%
  group_by(flight)%>%
  filter(dep_delay==10))
#A flight is 30 minutes early 50% of the time, and 30 minutes late 50% of the time.


delays<-not_cancelled%>%
    group_by(flight)%>%
    summarise(
      num_flights = n(),
      dep_30min = sum(dep_delay>30|dep_delay< -30)/num_flights
    )
   
delays%>%filter(dep_30min>0.5)


flight_delay_summary <- group_by(not_cancelled, flight) %>% summarise(num_flights = n(),
                                                                percentage_on_time = sum(arr_time == sched_arr_time)/num_flights,
                                                                percentage_early = sum(arr_time < sched_arr_time)/num_flights, 
                                                                percentage_15_mins_early = sum(sched_arr_time - arr_time == 15)/num_flights,
                                                                percentage_late = sum(arr_time > sched_arr_time)/num_flights,
                                                                percentage_15_mins_late = sum(arr_time - sched_arr_time == 15)/num_flights,
                                                                percentage_2_hours_late = sum(arr_time - sched_arr_time == 120)/num_flights)
flight_delay_summary %>% filter(percentage_15_mins_early == 0.5 & percentage_15_mins_late == 0.5)












#Come up with another approach that will give you the same output as not_cancelled %>% count(dest) and not_cancelled %>% count(tailnum, wt = distance) (without using count()).

op1<-not_cancelled %>% count(dest)
op2<-not_cancelled %>% count(tailnum, wt = distance)

op3for1<- not_cancelled%>%
  group_by(dest)%>%
  summarise(n = n())
  
?tally()

identical(op1, op3for1)

op4for2<- not_cancelled%>%
  group_by(tailnum)%>%
  summarise(
    n=sum(distance)
  )

identical(op2, op4for2)

#Our definition of cancelled flights (is.na(dep_delay) | is.na(arr_delay) ) is slightly suboptimal. 
#Why? Which is the most important column?

###The most important is dep_delay, because if one implies the other



#Look at the number of cancelled flights per day. Is there a pattern? 
#Is the proportion of cancelled flights related to the average delay?
colnames(flights)
cancelled <- flights%>%
  filter(is.na(dep_delay))
cancelled

flights%>%
  group_by(year, month, day)%>%
  summarise(
    cancelled = sum(is.na(dep_delay)),
    n = n(),
    mean_dep_delay = mean(dep_delay, na.rm = TRUE),
    mean_arr_delay = mean(arr_delay, na.rm = TRUE),
    prop_cancelled = cancelled/n

  )%>%
  ggplot(mapping = aes(y = prop_cancelled))+
  geom_smooth(mapping = aes(x =mean_dep_delay ), se = FALSE)+
  geom_smooth(mapping = aes(x = mean_arr_delay), se = FALSE)
###The higher the avg delat, the higher the proportion of cancelled flights. 
###this could mean that whatever causes the delay, may cause the cancellation,which is reasonable  to think

#Which carrier has the worst delays? (Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n())
colnames(not_cancelled)
not_cancelled%>%
  group_by(carrier)%>%
  summarise(
    n= n(),
    mean_dep_delay = mean(dep_delay, na.rm = TRUE),
    mean_arr_delay = mean(arr_delay, na.rm = TRUE)
  )%>%
  arrange(desc(mean_dep_delay), desc(mean_arr_delay))

#Challenge: can you disentangle the effects of bad airports vs. bad carriers? Why/why not? 
flights %>% 
  group_by(carrier) %>% 
  summarise(
    mean_dep_delay = mean(dep_delay, na.rm = TRUE)
  )%>%
  filter(carrier=="9E")
##9E avg delay is 16.7

flights %>% 
  group_by(carrier, dest) %>% 
  summarise(
    mean_dep_delay = mean(dep_delay, na.rm = TRUE),
    disentangled = mean_dep_delay-16.7
    )%>%
  filter(carrier=="9E")%>%
  arrange(desc(disentangled))

##up to this point we can see that for example for carrier 9E and dest BGR has the worst avg, almost doubling the avg delay of the carriera
##what would happen if I took the avg delay of the dest, I would expect that BGR has one of the worst results

flights %>% 
  group_by(dest) %>% 
  summarise(
    mean_dep_delay = mean(dep_delay, na.rm = TRUE)
  )
##As expected, BGR, independently of the carrier, is the top10 of worst results
#What does the sort argument to count() do. When might you use it?

flights%>%
  count(carrier == "9E")

vignette("window-functions")


batting <- Lahman::Batting %>%
  as_tibble() %>%
  select(playerID, yearID, teamID, G, AB:H) 
?semi_join()  

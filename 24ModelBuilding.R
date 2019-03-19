#24 Model Building
library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)

#24.2.3 Exercises

#1.In the plot of lcarat vs. lprice, there are some bright vertical strips. What do they represent?

##They represent  a major quantity of observations in those bins. And that probably happens due to the rounding of the carat.
diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))
ggplot(diamonds2, aes(lcarat, lprice)) + 
  geom_hex(bins = 50)
#2.If log(price) = a_0 + a_1 * log(carat), what does that say about the relationship between price and carat?
mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)
##Log transformations help to see the log relationship between two variables
##as it were a linear relationship. It says that there is a logarithmic relationship
##between the variables, meaning, each time x*x=>y*y


#3.Extract the diamonds that have very high and very low residuals. Is there anything unusual about these diamonds? Are they particularly bad or good, or do you think these are pricing errors?

grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)


diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")

#lresid>(2*mean(lresid))
diamonds2h<-diamonds2%>%arrange(desc(lresid))%>%head(20)
diamonds2t<-diamonds2%>%arrange(desc(lresid))%>%tail(20)
diamonds2n<-as.tibble(rbind(diamonds2h,diamonds2t))

ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
##Maby considering the cut, there are some "good" cut diamonds thar are badly priced
ggplot(diamonds2n, aes(cut, lresid)) + geom_boxplot()

##Acording to these plots, there are not badly priced ones
ggplot(diamonds2n, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2n, aes(clarity, lresid)) + geom_boxplot()



#4.Does the final model, mod_diamond2, do a good job of predicting diamond prices? Would you trust it to tell you how much to spend if you were buying a diamond?
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2")

res_analysis<-diamonds2%>%
  mutate(lresidabs = abs(lresid2))%>%
  arrange(desc(lresidabs))%>%
  select(lresidabs)

res_analysis%>%ggplot()+geom_histogram(aes(lresidabs))
##Considering that it makes a pretty good job. consiering only 4 obs have a residual with an absolute
##value > 2 and the histogram of abs values of residuals says they are all really close to 0



#24.3.5 Exercises

#1.Use your Google sleuthing skills to brainstorm why there were fewer than expected
#flights on Jan 20, May 26, and Sep 1. (Hint: they all have the same explanation.) How would these days generalise to another year?

##The three of them are sundays before holidays.Martin Luther King Jr. Day, Memorial Day, and Labor Day

#2.What do the three days with high positive residuals represent? How would these days generalise to another year?

daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())

daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))

mod <- lm(n ~ wday, data = daily)

daily <- daily %>% 
  add_residuals(mod)


term <- function(date) {
  cut(date, 
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall") 
  )
}

daily <- daily %>% 
  mutate(term = term(date)) 


daily %>% 
  top_n(3, resid)
##The top three days correspond to the Saturday after Thanksgiving (November 30th), the Sunday after Thanksgiving (December 1st), and the Saturday after Christmas (December 28th).


#3.Create a new variable that splits the wday variable into terms, but only for Saturdays, i.e. it should have Thurs, Fri, but Sat-summer, Sat-spring, Sat-fall. How does this model compare with the model with every combination of wday and term?
daily2<-daily%>%
  mutate(wday_term = case_when(
    wday == "Sat" ~ paste(wday, term),
    T~paste(wday)
      )
    )
mod3 <- MASS::rlm(n ~ wday * term, data = daily)

mod4 <- MASS::rlm(n ~ wday_term, data = daily2)

daily2<-daily2 %>% 
  add_residuals(mod3, "resid") %>%
  add_residuals(mod4, "resid_m")
daily2%>%ggplot()+
  geom_histogram(aes(resid_m), color = "green", alpha = 0.5)+
  geom_histogram(aes(resid), color = "blue", alpha = 0.5)
  
##There is a difference. The spread around 0 in slightly more in mod3, but it's
##hard to see a significative differene 
daily2<-daily2%>%
  spread_residuals(sat_term = mod4, all_interact = mod3)%>%
  mutate(resid_dif = sat_term - all_interact)
daily2%>%ggplot(aes(date, resid_dif))+geom_line()
##The difference is high in July, probabbly because summer vacations. In which
##people might be more prone to flight in a saturdays


#4.Create a new wday variable that combines the day of week, term (for Saturdays), and public holidays. What do the residuals of that model look like?
holidays_2013 <-
  tribble(
    ~holiday, ~date,
    "New Year's Day", 20130101,
    "Martin Luther King Jr. Day", 20130121,
    "Washington's Birthday", 20130218,
    "Memorial Day", 20130527,
    "Independence Day", 20130704,
    "Labor Day", 20130902,
    "Columbus Day", 20131028,
    "Veteran's Day", 20131111,
    "Thanksgiving", 20131128,
    "Christmas", 20131225
  ) %>%
  mutate(date = lubridate::ymd(date))
daily3<-daily2%>%
  left_join(holidays_2013)
daily3$holiday[is.na(daily3$holiday)]<-0
daily3$holiday[daily3$holiday!=0]<-1
daily3$holiday<-as.logical(as.numeric(daily3$holiday))
names(daily3)
daily3<-daily3%>%
  mutate(wth = paste(wday, term, holiday))


mod5 <- MASS::rlm(n ~ wth, data = daily3)
daily3<-daily3%>%
  add_residuals(mod5, "resid5")
daily3%>%ggplot(aes(date, resid5))+geom_line()
daily3%>%ggplot()+geom_histogram(aes(resid5), alpha = 0.5)+
  geom_histogram(aes(resid_m), alpha = 0.5, color = "green")

#5.What happens if you fit a day of week effect that varies by month (i.e. n ~ wday * month)? Why is this not very helpful?
daily4<-daily3%>%
  mutate(month=month(date, label = T))
mod6 <- MASS::rlm(n ~ wday*month, data = daily4)
daily4<-daily4%>%add_residuals(mod6, "resid")
daily4%>%ggplot()+geom_histogram(aes(resid))
daily4%>%ggplot(aes(date, resid))+geom_line()

##Well the problem with this is that this overfits the data because it considers every combination of wday and month
daily4%>%ggplot()+geom_histogram(aes(resid))+geom_histogram(aes(resid_m), alpha = 0.5, color = "green")
##Here we see that the distribution of residuals is much much higher close to 0


#6.What would you expect the model n ~ wday + ns(date, 5) to look like? Knowing what you know about the data, why would you expect it to be not particularly effective?

##Previous model was:
library(splines)
mod7h <- MASS::rlm(n ~ wday * date, data = daily)
mod7 <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)
##Which account for the interralation of each splin
mod8 <- MASS::rlm(n ~ wday + ns(date, 5), data = daily)
##Considers it independent of each other, which caues each curve to have the same 
##fit only with differents constant summed to it. What is interesting about 


daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod8) %>% 
  ggplot(aes(date, pred, colour = wday)) + 
  geom_line() +
  geom_point()

s<-c(1,3,4,6)
ns(s,2)
##he model wday + ns(date, 5) does not interact the day of week effect (wday) with the time of year effects (ns(date, 5)).

#7.We hypothesised that people leaving on Sundays are more likely to be business travellers who need to be somewhere on Monday. Explore that hypothesis by seeing how it breaks down based on distance and time: if it’s true, you’d expect to see more Sunday evening flights to places that are far away.

##This doesn't show this:
#(if it’s true, you’d expect to see more Sunday evening flights to places that are far away.)

flights %>%
  mutate(
    date = make_date(year, month, day),
    wday = wday(date, label = TRUE)
  ) %>%
  ggplot(aes(y = distance, x = wday))+
  geom_boxplot()


#8.It’s a little frustrating that Sunday and Saturday are on separate ends of the plot. Write a small function to set the levels of the factor so that the week starts on Monday
library(forcats)
monday_first <- function(x) {
  fct_relevel(x, c("Mon","Tue" ,"Wed" ,"Thu" ,"Fri" ,"Sat", "Sun"))
}

monday_first(daily8$wday)

daily8<-flights %>%
  mutate(
    date = make_date(year, month, day),
    wday = wday(date, label = TRUE)
  )

##Testing
daily8 %>%
  mutate(
    date = make_date(year, month, day),
    wday = monday_first(wday(date, label = TRUE))
  ) %>%
  ggplot(aes(y = distance, x = wday))+
  geom_boxplot()

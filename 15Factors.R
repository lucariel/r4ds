#15 Factors

library(tidyverse)
library(forcats)


#15.3.1 Exercise

#1.Explore the distribution of rincome (reported income). What makes 
#the default bar chart hard to understand? How could you improve the plot

#Factor to the values
View(gss_cat$rincome)

ggplot(gss_cat, aes(rincome)) +
  geom_bar()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

gss_cat%>%
  count(rincome)

income_level<-c("Not applicable", "No answer", "Don't know", "Refused", "Lt $1000",
                "$1000 to 2999","$3000 to 3999","$4000 to 4999","$5000 to 5999",
                "$6000 to 6999","$7000 to 7999","$8000 to 9999","$10000 - 14999","$15000 - 19999",
                "$20000 - 24999","$25000 or more")
gss_cat2<-gss_cat%>%
  mutate(
    rincome_factor = factor(rincome, levels = income_level)
  )

ggplot(gss_cat2, aes(rincome_factor)) +
  geom_bar()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##I still think it coul look better taking out Not applicable
income_level2<-c("No answer", "Don't know", "Refused", "Lt $1000",
                "$1000 to 2999","$3000 to 3999","$4000 to 4999","$5000 to 5999",
                "$6000 to 6999","$7000 to 7999","$8000 to 9999","$10000 - 14999","$15000 - 19999",
                "$20000 - 24999","$25000 or more")
gss_cat3<-gss_cat%>%
  mutate(
    rincome_factor = factor(rincome, levels = income_level2)
  )%>%
  filter(!is.na(rincome_factor))

ggplot(gss_cat3, aes(rincome_factor)) +
  geom_bar()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
##or maby all non numerical values
income_level3<-c("Lt $1000",
                 "$1000 to 2999","$3000 to 3999","$4000 to 4999","$5000 to 5999",
                 "$6000 to 6999","$7000 to 7999","$8000 to 9999","$10000 - 14999","$15000 - 19999",
                 "$20000 - 24999","$25000 or more")
gss_cat4<-gss_cat%>%
  mutate(
    rincome_factor = factor(rincome, levels = income_level3)
  )%>%
  filter(!is.na(rincome_factor))

ggplot(gss_cat4, aes(rincome_factor)) +
  geom_bar()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#2.What is the most common relig in this survey? What’s the most common partyid?
names(gss_cat)
dnoems<-gss_cat%>%
  count(relig)
dnoems
##Protestant

gss_cat%>%
  count(partyid)

##Independent

#3.Which relig does denom (denomination) apply to? 
#How can you find out with a table? How can you find out with a visualisation?
##I could watch it here
View(gss_cat)
##But a more thorough analysis would be

gss_cat6<-gss_cat%>%
  filter(relig!="Protestant")%>%
  count(denom)
View(gss_cat6)
##If I filter out the protestant, denom entries are meaningless
##c(No answer,Don't know,No denomination,Not applicable)

## with a visualisation you could make a facet plot 
##First change de levels of relig
gss_cat7<-gss_cat%>%
  mutate(
    relig_factor = factor(relig, levels = c("Protestant"))
  )
gss_cat7%>%ggplot(aes(denom))+geom_bar()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(gss_cat7$relig_factor)

#15.4 Modifying factor order

#For example, imagine you want to explore the average number of hours spent watching TV per day across religions:
  
  relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary, aes(tvhours, relig)) + geom_point()

##reordered
ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours)))+geom_point()






#What if we create a similar plot looking at how average age varies 
#across reported income level?

rincome_summary<-gss_cat%>%
  group_by(rincome)%>%
  summarise(
    age = mean(age, na.rm = T),
    tvhours = mean(tvhours, na.rm = T),
    n = n()
  )
ggplot(rincome_summary, aes(age, fct_reorder(rincome, age)))+geom_point()
?fct_relevel()


#15.4.1 Exercises

#1.There are some suspiciously high numbers in tvhours. Is the mean a good summary?

gss_cat%>%
  ggplot(aes(tvhours))+
  geom_histogram(na.rm = T)
##It can be, once the outlier is removed
gss_cat8<-gss_cat%>%
  filter(
    tvhours<15
  )
gss_cat8%>%
  ggplot(aes(tvhours))+
  geom_histogram(na.rm = T)
#2.For each factor in gss_cat identify whether the order of the levels is arbitrary or principled.

glimpse(gss_cat)
##Marital: Arbitrary {A}
##Race: A
##Rincome: Principled {P}
##Partyid: A
##relig: A
##denom: A

#3.Why did moving “Not applicable” to the front of the levels move it to the bottom of the plot?
ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable", after = Inf))) +
  geom_point()
levels(rincome_summary$rincome)
?fct_relevel()

##There was the parameter "after=" missing which tells the function where to put it.





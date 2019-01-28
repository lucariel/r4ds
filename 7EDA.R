############7 Exploratory Data Analysis############
library(tidyverse)
library(ggplot2)




# “There are no routine statistical questions, only questionable statistical routines.” — Sir David Cox
#
# “Far better an approximate answer to the right question, which is often vague, than an exact answer to the wrong question, which can always be made precise.” — John Tukey

##During an EDA the important part is to dig up the right questions to the data, is a creative process.
##Each new question will expose a new aspect, just like a new powl does to a hole.
##There is no determined path, but starting to ask  about the variations of the variables in themselves and with other variables is always a way to start
##Here is good to define the terms:
####variable: a mesaureble entity
####value: the mesaure taken to the variable.
####observation: set of meseaurments made under similar conditions. sometimes: datapoint
####tabular data: set of values - it's tidy if each value is in its own cell


#7.3 Variation

##Consider that each value has it's own variation pattern even the constants. The best way to understand that pattern is to visualise the distribution of the variable’s values.


#7.3.1 Visualising distributions
##Variables can be  categorical or continuous.

##Example categorical (small set of values)
ggplot(data = diamonds)+
  geom_bar(mapping = aes(x= cut))

diamonds%>%
  count(cut)

##Continuous (infinite set of values)

ggplot(data = diamonds)+
  geom_histogram(mapping = aes(x=carat), binwidth = .5)
##The histogram is usefull because it graphs in bins, if you were to use geom_bar()
ggplot(data = diamonds)+
  geom_bar(mapping = aes(x= carat))
##It would show every value and its freq, but there are infinite possible values so its good to group them toghether with bins and graphic the bins
##
ggplot(data = diamonds)+
  geom_histogram(mapping = aes(x=carat), binwidth = .1)+
  geom_smooth()

?geom_smooth()
#7.3.2 Typical values

##To ask questions to histograms:
###Which values are the most common? Why? which rare? why?
###Pattern emerge?
###Example:
smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)
###more diamonds at whole carats and common fractions of carats?
###more diamonds slightly to the right of each peak than there are slightly to the left of each peak?
###no diamonds bigger than 3 carats?


####This question lead us to conclude the existence of subgroups in the data
####to understand them one can ask:

###Ok the are clusters of data (in this case, around the hole numbers)
###what does the datapoints have in common, what does the cluster differ?
###Can the clusters be misleading?

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.5)
###Ok, this graph suggests that there are two peaks, one around 2min and one at around 4

#7.3.3 Unusual values
##Outliers are extreme values, sometimes they are errors, but other times they
##can reveal certain characterstics of the data

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)
###If we zoom in:
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
?coord_cartesian()#this added changes the scale, zooming (limit Y to 50)
###Outliners apear!
##It’s good practice to repeat your analysis with and without the outliers. 



#7.3.4 Exercises

#1.Explore the distribution of each of the x, y, and z variables in diamonds. What do you learn? Think about a diamond and how you might decide which dimension is the length, width, and depth.

diamonds
ggplot(diamonds)+
  geom_histogram(mapping = aes(x = x), binwidth = 0.5)

ggplot(diamonds)+
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)+
  coord_cartesian(ylim = c(0, 50))


ggplot(diamonds)+
  geom_histogram(mapping = aes(x = z), binwidth = 0.5)+
  coord_cartesian(ylim = c(0, 50))

##y and z have outliners. Let's remove them so they dont disturb the analysis
diamonds_filtered<-diamonds%>%
  filter(y<20, z<20)

ggplot(diamonds_filtered)+
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)+
  geom_freqpoly(mapping = aes(x = y), binwidth=0.5)
ggplot(diamonds_filtered)+
  geom_histogram(mapping = aes(x = z), binwidth = 0.5)
ggplot(diamonds_filtered)+
  geom_histogram(mapping = aes(x = x), binwidth = 0.5)


ggplot(diamonds_filtered)+
  geom_freqpoly(mapping = aes(x = x), binwidth=0.5, color = "blue")+
  geom_freqpoly(mapping = aes(x = y), binwidth=0.5, color = "red")+
  geom_freqpoly(mapping = aes(x = z), binwidth=0.5, color = "black")
##Now we have that x and y take similar values and frequencies. So I would say
##that about diamonds, length and width are similar, so they could be that. and Z
##would be depth.

#2.Explore the distribution of price. Do you discover anything unusual or surprising? (Hint: Carefully think about the binwidth and make sure you try a wide range of values.)
ggplot(data = diamonds_filtered)+
  geom_freqpoly(mapping = aes(x = price), binwidth = 1000, color = "red")
##One thing I notice is the hugh pike at the begining, lets zoom in to see what happens
diamonds_f2<-diamonds%>%
  filter(price>50 & price<1500 )
colnames(diamonds)

ggplot(data = diamonds_f2)+

geom_freqpoly(mapping = aes(x = price), binwidth = 20)#Ok doesnt make a difference

#3.How many diamonds are 0.99 carat? How many are 1 carat? What do you think is the cause of the difference?

d_carat_1<-diamonds%>%
  filter(carat==0.99)
length(d_carat_1$carat)


d_carat_2<-diamonds%>%
  filter(carat==1)
length(d_carat_2$carat)
##This could be that 0.99 carat diamonds could be rounded up to nearest integer


#4.Compare and contrast coord_cartesian() vs xlim() or ylim() when zooming in on a histogram. What happens if you leave binwidth unset? What happens if you try and zoom so only half a bar shows?

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y)) +
  coord_cartesian(xlim = c(0, 50))
##Standar bin = 30


#7.4 Missing values
##Options:
##1.Drop entire row with missing values (suboptimal)
diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))
##2.Replacing the unusual values with missing values.
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))
######Ok lets take a moment to talk about ifelse()
############ifelse(logicalVector, caseTrue, caseFalse)
##Plotting the replaced values
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()
##This gives a warning about the missing values, to removethem manually
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm=T)

##Ok thats all well and good, but what if I want to analyse what makes them NA
##you could make a new variable with is.na()

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

 
#1. What happens to missing values in a histogram? What happens to missing values in a bar chart? Why is there a difference?
diamonds %>%
  ggplot(aes(price)) +
  geom_bar()
##We can see that there is a gap in the plot, corresponding to the NA values
#2.What does na.rm = TRUE do in mean() and sum()?

testsum<-c(1,2,4, NA)
sum(testsum, na.rm = T)
##Without the na.rm = T, neither of the functions can be processed, the function returns NA if there is a any NA value

#7.5 Covariation
##Covariation is the tendency for the values of two or more variables to vary together in a related way
#Let’s take a look at the distribution of price by cut using geom_boxplot():
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()+
  coord_flip()


#1.Use what you’ve learned to improve the visualisation of the departure times of cancelled vs. non-cancelled flights.

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_boxplot(mapping = aes(x = cancelled, y = sched_hour ))
##As we can see in the plot, the cancelled flights follow a more normal distribution, less skewed.

#2.What variable in the diamonds dataset is most important for predicting the price of a diamond? How is that variable correlated with cut? Why does the combination of those two relationships lead to lower quality diamonds being more expensive?

ggplot(data = diamonds, mapping = aes(x = x, y = price))+
  geom_point(na.rm = T)+
  geom_smooth(na.rm = T, se =F)+ scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2')
##The most conving variable I see is X which in turn correles a lot with carat.
##In any case let's see the analysis next to cut

ggplot(data = diamonds, mapping = aes(x = cut, y = x)) +
  geom_boxplot()


##Seing every of this distributions, it seams to me that fair has a higher average price because it's more consistent in it's size and carat
##Looking at https://github.com/jrnold/r4ds-exercise-solutions/blob/master/EDA.Rmd answers
##Looks like we found the same but there is a little idfference in interpretations


#3.Install the ggstance package, and create a horizontal boxplot. How does this compare to using coord_flip()?

install.packages("ggstance")
##from the docs:
##ggstance provides flipped versions of Geoms, Stats and Positions. This makes it easier to build horizontal layer or use vertical positioning (e.g. vertical dodging). Also, horizontal Geoms draw horizontal legend keys to keep the appearance of your plots consistent.

#4. Install the lvplot package, and try using geom_lv() to display the distribution of price vs cut. What do you learn? How do you interpret the plots?
install.packages("lvplot")
library("lvplot")
ggplot(data = diamonds) +
  geom_lv( mapping = aes(x = price, y = price))


#7.5.2 Two categorical variables
##To visualise the covariation between categorical variables, its useful to count the instances of each

ggplot(data = diamonds)+
  geom_count(mapping = aes(x = cut, y = color))
##This way we can see that the size depends on the correlation of each coincidence of cut and color
##to see this without a plot we can:
diamonds%>%
  count(color, cut)
##Adding a plot geom_tile()

diamonds%>%
  count(color, cut)%>%
  ggplot(mapping = aes(x = cut, y = color))+
  geom_tile(mapping = aes(fill = n))

#1.How could you rescale the count dataset above to more clearly show the distribution of cut within colour, or colour within cut?

ggplot(diamonds, aes(carat)) +
  geom_histogram() +
  facet_wrap(~cut, scales = "free")

diamonds%>%
  count(color, cut)%>%
  group_by(color)%>%
  mutate(prop = n/sum(n))%>%
  ggplot(mapping = aes(x = color, y = cut))+
  geom_tile(mapping =aes(fill=prop))
#Use geom_tile() together with dplyr to explore how average flight delays vary by destination and month of year. What makes the plot difficult to read? How could you improve it?
library(nycflights13)
colnames(flights)


flights_s<-flights%>%
  group_by(month)%>%
  mutate(season = ifelse(month %in% c(1,2,3),"winter",
                         ifelse(month %in% c(4,5,6), "spring",
                                ifelse(month %in% c(7,8,9), "summer",
                                       "autumm"
                                       )
                                )
                         )
         )
View(flights_s)

flights_m<-flights_s%>%
  group_by(dest, season)%>%
  mutate(avg_delay = mean(dep_delay, na.rm = T))%>%
  count(dest, season,avg_delay, na.rm =T)
##There are too many destinations

##If I filter by n>2000 and reduce the month toseason, the tile geom is more understandable. Still there are too many destinations
flights_m%>%
  filter(n>2000)%>%
  ggplot(mapping = aes(x = dest, y = season))+
  geom_tile(mapping =aes(fill=avg_delay))
View(flights_m)

##I wonder why there is an empy square in the tile

#
# 
#3. Why is it slightly better to use aes(x = color, y = cut) rather than aes(x = cut, y = color) in the example above?
#   
diamonds%>%
  count(color, cut)%>%
  group_by(color)%>%
  mutate(prop = n/sum(n))%>%
  ggplot(mapping = aes(x = cut, y = color))+
  geom_tile(mapping =aes(fill=prop))
##It's slightly better becaue you get the lowest prop down




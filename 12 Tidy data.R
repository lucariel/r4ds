##12 Tidy data##

library(tidyverse)


#1.Using prose, describe how the variables and observations are organised in each of the sample tables.
table1
table2
table3
table4a
table4b


##Considering that tidy data has this :
##

# Each variable must have its own column.
# Each observation must have its own row.
# Each value must have its own cell.

##The variables are country, year, cases and population. only table1 has this format, only table1 is tidy
table2
##Table 2 has variables in rows, the column "type" should be split 
table3
##Table 3 is a mess: numerical variables are char because its displayed as a ratio. We have to columns badly merged. If we wanted to have only the variable rate it would be better to get a transformed table from a tidy dataset which would include a column for each variable

table4a

table4b# each row represents two observations, not one.
##Tables 4s are separeted when there is no need to be so, and there are observations as columns, which should not be the case.

#2.Compute the rate for table2, and table4a + table4b. You will need to perform four operations:
# 
# Extract the number of TB cases per country per year.
# Extract the matching population per country per year.
# Divide cases by population, and multiply by 10000.
# Store back in the appropriate place.
# 
# Which representation is easiest to work with? Which is hardest? Why?

##For table 2
table2a<-table2%>%filter(type =="cases")
table2b<-table2%>%filter(type =="population")
table1
table2c<-tibble(
  country = table2a$country, 
  year = table2a$year,
  cases=table2a$count, 
  population = table2b$count)%>%
  mutate(ratio = (cases/population)*1000)


##For tables 4a, 4b
table4a
table4c<-tibble( 
  country = c(table4a$country,table4a$country), 
  cases=c(table4a$`1999`, table4a$`2000`), 
  population = c(table4b$`1999`, table4b$`2000`), 
  year = c(1999,1999,1999,2000,2000,2000))

table4c<-table4c%>%mutate(ratio = cases/population*1000)%>%arrange(country)

identical(table4c$ratio, table2c$ratio)

#Which representation is easiest to work with? Which is hardest? Why?
##Of both 2 and 4, it was easer with the 2 (variables in rows) than 4 (observations as columns). It was easier to separete the rows because the observations can have much more variations make it more difficult to work with

#3.Recreate the plot showing change in cases over time using table2 instead of table1. What do you need to do first?
  
library(ggplot2)
ggplot(table2c, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))




#12.3.1 Gathering

##To tidy data from table4a
table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")

##To tidy and combine tables 4a and 4b

tidy4a <- table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b)

# 
# 12.3.2 Spreading
# 
# Spreading is the opposite of gathering

table2 %>%
  spread(key = type, value = count)

#1.Why are gather() and spread() not perfectly symmetrical? Carefully consider the following example:
  
stocks <- tibble(
    year   = c(2015, 2015, 2016, 2016),
    half  = c(   1,    2,     1,    2),
    return = c(1.88, 0.59, 0.92, 0.17)
  )

stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`)

s1<-stocks %>% 
  spread(year, return)

s1%>%gather(`2015`,`2016`, key = "year", value = "return")
##The functions are not perfectly simmetrical, when the code

stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`)

##runs, the type of the year vector is "char" not "dbl" as it was originally
##this happens because when spread the column names are non-syntactic names so
##the type of the variable is not kept. 

#2.Why does this code fail?

table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")%>%
  mutate(year = as.numeric(year))
#> Error in inds_combine(.vars, ind_list): Position must be between 0 and n


#3.Why does spreading this tibble fail? How could you add a new column to fix the problem?

people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
# 
people %>%
  spread(key = key, value = value)
# Spreading the data frame fails because there are two rows with "age" for "Phillip Woods".


people <- tribble(
  ~name,             ~key,    ~value, ~obs,
  #-----------------|--------|------|------
  "Phillip Woods",   "age",       45, 1,
  "Phillip Woods",   "height",   186, 1,
  "Phillip Woods",   "age",       50, 2,
  "Jessica Cordero", "age",       37, 1,
  "Jessica Cordero", "height",   156, 1
)
spread(people, key, value)



#4.Tidy the simple tibble below. Do you need to spread or gather it? What are the variables?

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)

##variables are: 
####pregnant:"yes","no"
####sex:"male", "female"
####n>=0

preg%>%
  gather(male, female, key = "sex", value = "count")

# 
# 12.4.3 Exercises
# 
#1. What do the extra and fill arguments do in separate()? Experiment with the various options for the following two toy datasets.

?separate()

##Here fill doesnt work, only extra, which solves the problem of too many elements
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), fill = "right")


tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "drop")


##Here extra doesnt work, only fill, which solves the problem of not enough elements
tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), extra = "merge")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), fill = "left")



#2.Both unite() and separate() have a remove argument. What does it do? Why would you set it to FALSE?
tibble(x = c("a,b,c", "d,e,f", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), remove = F)

##The argument "remove" removes the original column and we are only left with the converted one, it's a similar situation for one might use mutate(FALSE) and transmutate (TRUE)


#3.Compare and contrast separate() and extract(). Why are there three variations of separation (by position, by separator, and with groups), but only one unite?

?separate()

?extract() ##Its seems to be more general, it has a regex parameter
tibble(x = c("a,b,c", "d,e,h", "f,g,i")) %>% 

  
  
##Missing values##
  
  
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)

stocks%>%
  spread(year, return)
?complete()



##
treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)


treatment%>%fill(person)
##Excercises##

##Compare and contrast the fill arguments to spread() and complete().

?spread()#fill	If set, missing values will be replaced with this value. Note that there are two types of missingness in the input: explicit missing values (i.e. NA), and implicit missings, rows that simply aren't present. Both types of missing value will be replaced by fill.

?complete()
#fill	A named list that for each variable supplies a single value to use instead of NA for missing combinations.


?fill()
#Direction in which to fill missing values. Currently either "down" (the default) or "up".


##12.6 Case Study

who
who1<- who%>%
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = T)
who1
##To this point, we can see that each key apears several times lets see how many

who1%>%count(key)

c_who1<-who1%>%count(key)
c_who1$key
who2<-who1%>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
c_who1<-who2%>%count(key)
c_who1$key
who3<-who2%>%
  separate(key, c("new", "type", "sexage"), sep = "_")
who3
who3%>%count(new)
who4<-who3%>%select(-new, -iso2, -iso3)
who4

who5<-who4%>%separate(sexage, c("sex", "age"), sep = 1)
who5%>%group_by(country)%>%
  summarize(tcases = sum(cases))

##1.In this case study I set na.rm = TRUE just to make it easier to check that we had the correct values. Is this reasonable? Think about how missing values are represented in this dataset. Are there implicit missing values? What’s the difference between an NA and zero?

who1<-who %>%
  gather(key, value, new_sp_m014:newrel_f65, na.rm = T) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)
who1$value
#To know wether it was a good idea to drop NA values its necesary to know the target of the analysis. The difference between NA and 0 is that NA means that the are explicit missing values, and 0 is not amissing value. 


##What happens if you neglect the mutate() step? (mutate(key = stringr::str_replace(key, "newrel", "new_rel")))

who %>%
  gather(key, value, new_sp_m014:newrel_f65, na.rm = T) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)
#the data wouldn't finish to be tidy if that was done. it was necesary step otherwis, when we separate it, there would be a differene between n of columns and n of rows. And the separation would have been more complicated.

##I claimed that iso2 and iso3 were redundant with country. Confirm this claim.
#They are reduntand because all of the 3 columns are about identify the country, so we would not need them


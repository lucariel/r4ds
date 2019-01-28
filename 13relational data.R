library(tidyverse)
library(nycflights13)
airlines
airports
planes
weather
flights
#13.1)Imagine you wanted to draw (approximately) the route each plane flies from its origin to its destination. What variables would you need? What tables would you need to combine?
##Ok, to draw them in a map we would need the location of the airports departure and arrival and the flights. The flights tibble has later, and the airports the former.
colnames(weather)
colnames(airports)


weather%>%count(origin)
airports%>%filter(faa=="EWR"| faa == "JFK"| faa == "LGA")
#13.3)weather only contains information for the origin (NYC) airports. If it contained weather records for all airports in the USA, what additional relation would it define with flights?
##Origin 


#13.4)We know that some days of the year are “special”, and fewer people than usual fly on them. How might you represent that data as a data frame? What would be the primary keys of that table? How would it connect to the existing tables?
##First I would have to identify which ones are they, if they were the holidays I could create a tibble which would name them and woud relate with columns year, month and year as a primarykey.



#13.3 Keys

##To check that what is considered primary key is in fact, unique you can count

planes%>%
  count(tailnum)%>%
  filter(n>1)


##What abount in flights
flights%>%
  count(year, month, day, flight)%>%
  filter(n>1)
#13.3.1 Exercises

#1)Add a surrogate key to flights.

flights%>%
  mutate(id = row_number())%>%
  count(id)



library(Lahman)
Batting

colnames(Batting)
Batting%>%count(playerID, yearID, teamID, lgID, G)%>%filter(n>1)
##Lahman::Batting PK is:  playerID, yearID, stint 
library(babynames)
?babynames
babynames%>%count(name, year, sex)%>%filter(nn>1)

##babynames::babynames PK is:  name, year, sex


library(nasaweather)
colnames(atmos)
?atmos
atmos%>%count(lat, long, year, month)%>%filter(n>1)

##atmos PK is:  lat, long, year, month


library(fueleconomy)
?vehicles
colnames(vehicles)
vehicles%>%count(id)%>%filter(n>1)
##vehicles PK is : ID

library(ggplot2)

?diamonds
diamonds%>%count(x,y,z, price)%>%filter(n>1)
##there is no primary key, I first thought it would be the description x,y,z since I doubted that there would be 2 diamonds of the same size
ggplot2::diamonds %>%
  distinct() %>%
  nrow()
nrow(ggplot2::diamonds)



##
flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)


colnames(flights2%>%left_join(weather))

flights2 %>% 
  left_join(planes, by = "tailnum")
?planes


flights2 %>% 
  left_join(airports, c("dest" = "faa"))
flights2 %>% 
  left_join(airports, c("origin" = "faa"))

?airports
flights2$dest
# 13.4.6 Exercises
# 
#1. Compute the average delay by destination, then join on the airports data frame so you can show the spatial distribution of delays. Here’s an easy way to draw a map of the United States:

flights2<-flights%>%
  group_by(dest)%>%
  summarise(avg_del = mean(arr_delay, na.rm = T))
airports %>%
  inner_join(flights2, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point(aes(color= avg_del)) +
  coord_quickmap()


#2.Add the location of the origin and destination (i.e. the lat and lon) to flights.
airport_locations <- airports %>%
  select(faa, lat, lon)
flights%>%
  left_join(airport_locations, c("dest"="faa"))%>%
  left_join(airport_locations, c("origin" = "faa"))


#3.Is there a relationship between the age of a plane and its delays?

planes
flights$tailnum

planes_age<-planes%>%
  group_by(tailnum)%>%
  summarise(year)

flights_delays <-flights%>%
  group_by(tailnum, year)%>%
  summarise(avg_arr = mean(arr_delay, na.rm = T),
            sd_arr = sd(arr_delay, na.rm = T),
            avg_dep = mean(dep_delay, na.rm = T),
            sd_dep =sd(dep_delay, na.rm = T),
            n = n())

flights_delays%>%count()
planes_age%>%count()


planes_age%>%
  left_join(flights_delays, by = "tailnum", na.rm = T)%>%
  mutate(age = year.y - year.x)%>%
  group_by(age)%>%
  summarise(
    dep_del_avg = mean(avg_arr, na.rm = T)
  )%>%
  ggplot(aes(x= age, y = dep_del_avg))+
  geom_point()+ 
  scale_x_continuous("Age of plane (years)", breaks = seq(0, 50, by = 10)) +
  scale_y_continuous("Mean Departure Delay (minutes)")



plane_cohorts <- inner_join(flights,
                            select(planes, tailnum, plane_year = year),
                            by = "tailnum"
) %>%
  mutate(age = year - plane_year) %>%
  filter(!is.na(age)) %>%
  group_by(age) %>%
  summarise(
    dep_delay_mean = mean(dep_delay, na.rm = TRUE),
    dep_delay_sd = sd(dep_delay, na.rm = TRUE),
    arr_delay_mean = mean(arr_delay, na.rm = TRUE),
    arr_delay_sd = sd(arr_delay, na.rm = TRUE),
    n = n()
  )


#4.What weather conditions make it more likely to see a delay?

flights_delays2<-flights%>%
  group_by(tailnum, year)%>%
  summarise(avg_arr = mean(arr_delay, na.rm = T),
            sd_arr = sd(arr_delay, na.rm = T),
            avg_dep = mean(dep_delay, na.rm = T),
            sd_dep =sd(dep_delay, na.rm = T),
            n = n())
flights_weather<-flights%>%inner_join(weather, na.rm=T)
View(flights_weather)

?weather
flights_weather%>%
  filter(arr_delay>300)%>%
  ggplot(aes(x = precip , y = arr_delay ))+
  geom_point()
##Visibility and precipitations conditions cause delay but they 
  
##5.What happened on June 13 2013? Display the spatial pattern of delays, and then use Google to cross-reference with the weather.
colnames(flights_weather)
june1313<-flights_weather%>%filter(
  year== 2013,
  month == 06,
  day == 13
)
#The event that happened was June 12–13, 2013 derecho series (https://en.wikipedia.org/wiki/June_12%E2%80%9313,_2013_derecho_series)

june1313%>%
  group_by(dest, wind_speed)%>%
  summarise(delay = mean(arr_delay, na.rm = T))%>%
  inner_join(airports, by=c("dest"="faa"))%>%
  ggplot(aes(x=lon, y = lat, color = wind_speed, size = delay))+
  geom_point()+
  borders("state")+
  coord_quickmap()
colnames(pplot)
##13.5.1 Exercises
flights
planes

missing_tailnum<-flights%>%
  filter(is.na(tailnum))
#1.aWhat does it mean for a flight to have a missing tailnum? 
##Every NA tailnum corresponds to a NA data of the flight info

#1.bWhat do the tail numbers that don’t have a matching record in planes have in common?

non_planes<-flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)
flights%>%semi_join(non_planes)%>%count(carrier)

#2.Filter flights to only show flights with planes that have flown at least 100 flights.

f<-flights%>%
  mutate(n = 1)
View(f)

f%>%
  group_by(tailnum)%>%
  summarise(q_flights = sum(n))%>%
  filter(q_flights>100)


#3.Combine fueleconomy::vehicles and fueleconomy::common to find only the records for the most common models.
library(fueleconomy)
View(vehicles)
View(common)
vehicles%>%inner_join(common, by = c("model"="model"))


#4.Find the 48 hours (over the course of the whole year) that have the worst delays. Cross-reference it with the weather data. Can you see any patterns?

f<-flights%>%
  group_by(month, day)%>%
  summarise(delay = mean(dep_delay, na.rm = T))%>%
  arrange(-delay)

##The most delays were the 8 of march and the first  of july

#5.What does anti_join(flights, airports, by = c("dest" = "faa")) tell you? What does anti_join(airports, flights, by = c("faa" = "dest")) tell you?
d<-anti_join(flights, airports, by = c("dest" = "faa"))
##Since anti_joins keeps the rows that dont have a match, this tells us which
##are the flights which have as destination an airport which is not on the arports database, which aparentily doesnt include puerto rico and the virgin islands


d%>%count(dest)
#6.You might expect that there’s an implicit relationship between plane and airline, because each plane is flown by a single airline. Confirm or reject this hypothesis using the tools you’ve learned above.

plane_carriers<-
  flights%>%
  filter(!is.na(tailnum))%>%
  distinct(tailnum, carrier)


duplicaded<-plane_carriers%>%
  count(tailnum)%>%
  filter(n>1)
plane_carriers
semi_join(plane_carriers)

flights_3<-flights%>%
  semi_join(plane_carriers)
View(flights_3)

flights_3%>%
  filter(tailnum=="N14228")%>%View()

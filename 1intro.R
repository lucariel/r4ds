 ### R for data science
library(tidyverse)
library(funModeling)
library(ggplot2)
library(MASS)
library(ggplot2)
library(viridis)



## 3.8 Position Adjustments

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x = cut, color = cut))
ggplot(data = diamonds)+
  geom_bar(mapping = aes(x = cut, fill = cut))

colnames(diamonds)

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x = cut, fill = color))
diamonds
?geom_bar()


ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "fill")

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity))+
  geom_bar(position = "dodge")


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(position = "jitter")

ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_jitter()



#3.8.1 Exercises
ggplot(data = mpg, mapping = aes(x = cty, y = hwy, color =  cty)) + 
  geom_jitter()

#What is the problem with this plot? How could you improve it?
#El problema del ploteo este es que, si bien nos proporciona una mirada de los datos, no sabemos 
#cuantos casos de cada uno hay, al igual que el anterior, se puede mejorar con geom_jitter
?geom_jitter()
#What parameters to geom_jitter() control the amount of jittering?
#Los parametros que nos dicen cuanto se jitterea es width y height


#Compare and contrast geom_jitter() with geom_count().
ggplot(data = mpg, mapping = aes(x = cty, y = hwy, color =  cty)) + 
  geom_count()
#ambos parecen ser buenas herramientas para visualizar problemas de overploting, uno 
#lo que hace es agregar ruido para poder visualizar mejor la densidad y el otro cuenta
#los casos de cada punto y les da un tamaño acorde.

#What’s the default position adjustment for geom_boxplot()? 
#Create a visualisation of the mpg dataset that demonstrates it.

?geom_boxplot()
#position = "dodge2" es el defaut, para poder demostrarlo, voy a darle otras posiciones
#para ver porque es así
ggplot(data = mpg, mapping = aes(x = drv, y = cty, fill = class))+
  geom_boxplot(position= "dodge")

ggplot(data = mpg, mapping = aes(x = drv, y = cty, fill = class))+
  geom_boxplot(position= "identity")

#causa overlaping, haciendolo ilegible

ggplot(data = mpg, mapping = aes(x = drv, y = cty, fill = class))+
  geom_boxplot(position= "jitter")

##genera error


#3.9 Coordinate systems
#Turn a stacked bar chart into a pie chart using coord_polar().
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")+
  coord_polar()
#What does labs() do? Read the documentation.
?labs()
p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
p + labs(colour = "Cylinders")
p + labs(x = "New x label")
#son las etiquetas que se le da a las variables para mejorar la legibilidad


#What’s the difference between coord_quickmap() and coord_map()?
?coord_quickmap() #coord_quickmap(xlim = NULL, ylim = NULL, expand = TRUE,
#clip = "on"), no permite especificar "projection" u otros paramtros.
#coord_quickmap is a quick approximation that does preserve straight lines
?coord_map() #coord_map(projection = "mercator", ..., parameters = NULL,
#orientation = NULL, xlim = NULL, ylim = NULL, clip = "on")


#La diferencia entre ambos son los argumentos que toma cada uno


  nz <- map_data("nz")
  # Prepare a map of NZ
  nzmap <- ggplot(nz, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "white", colour = "black")
  
  # Plot it in cartesian coordinates
  nzmap
  # With correct mercator projection
  nzmap + coord_map()
  # With the aspect ratio approximation
  nzmap + coord_quickmap()

  
 # What does the plot below tell you about the relationship between city and highway mpg?
  #Why is coord_fixed() important? What does geom_abline() do?
    
    ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
    geom_point() + 
    geom_abline() +
    coord_fixed()
?coord_fixed() # forces a specified ratio between the physical representation of data units on the axes
    
    ##Capitulo 2
    x<-"hello world"
    y<-seq(1,10, length.out = 4)
    y
    ?seq()
    # #Why does this code not work?
    # 
    my_variable <- 10
    my_varıable
    #> Error in eval(expr, envir, enclos): object 'my_varıable' not found
    #porque la ı es != i 
    # my_variable
    # Tweak each of the following R commands so that they run correctly: 
    
    library(tidyverse)
    
    ggplot(data = mpg) + 
      geom_point(mapping = aes(x = displ, y = hwy))
    colnames(mpg) 
    filter(mpg, cyl == 8)
    filter(diamonds, carat > 3)
    #Press Alt + Shift + K. What happens? How can you get to the same place using the menus?
    #aparecen todos los atajos
  
    
install.packages("nycflights13")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
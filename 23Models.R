#23 Model basics
library(tidyverse)
library(modelr)
library(ggplot2)
options(na.action = na.warn)


mod1f<-function(a,data){
  a[1]+data$x*a[2]
}



#23.2.1 Exercises

#1.One downside of the linear model is that it is sensitive to unusual values because the distance incorporates a squared term. Fit a linear model to the simulated data below, and visualise the results. Rerun a few times to generate different simulated datasets. What do you notice about the model?
  
sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
  )

sim1a_fil<-sim1a%>%filter(y<20)
mod1a<-lm(y~x, data = sim1a)
coefs_1a<-as.vector(coef(mod1a))
mod2a<-lm(y~x, data = sim1a_fil)
coefs_2a<-as.vector(coef(mod2a))
ggplot(sim1a, aes(x,y))+
  geom_point()+
  geom_abline(data=coefs_1a, slope = coefs_1a[2], intercept = coefs_1a[1], color = "red")+
  geom_abline(data=coefs_2a, slope = coefs_2a[2], intercept = coefs_2a[1])
##Outlier play a major rol in the slope of the model.


#2.One way to make linear models more robust is to use a different distance measure. For example, instead of root-mean-squared distance, you could use mean-absolute distance:

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1a)

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  mean(abs(diff))
}


best <- optim(c(0, 0), measure_distance, data = sim1a)
model1(best$par, sim1a)
coefs2<-best$par
ggplot(sim1a, aes(x,y))+
  geom_point()+
  geom_abline(data=coefs_1a, slope = coefs_1a[2], intercept = coefs_1a[1], color = "red")+
  geom_abline(data=coefs2, slope = coefs2[2], intercept = coefs2[1])
coefs_1a
coefs2

#Use optim() to fit this model to the simulated data above and compare it to the linear model.
##Well, it appears to have a steeper slope, and thus, less affected by the outlier.


#3.One challenge with performing numerical optimisation is that it’s only guaranteed to find one local optimum. What’s the problem with optimising a three parameter model like this?

model2 <- function(a, data) {
  a[1] + data$x * a[2] + a[3]
}

model2(c(0,0,0), sim1a)
best <- optim(c(0, 0,0), measure_distance, data = sim1a)
coefs3<-best$par

##The problem is that it will have the same fit, since what was before a[1] now is (a[1]+a[3])
##and won't change the slope. In this case the only way would be to a have a polynomial alternative.
##because the a[3] would be independent from "x" and thus, "x" wouldn't affect the outcome



#23.3.3 Exercises

#1.Instead of using lm() to fit a straight line, you can use loess() to fit a smooth curve. Repeat the process of model fitting, grid generation, predictions, and visualisation on sim1 using loess() instead of lm(). How does the result compare to geom_smooth()?

modsim2<-loess(y ~ x, data = sim1a)
sim1a <- sim1a %>% 
  add_predictions(modsim2)
sim1a%>%ggplot(aes(x, y))+
  geom_point()+geom_line(aes(y = pred))


sim1a<-sim1a%>%add_residuals(modsim2)

ggplot(sim1a, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)
#With a polynomial model we see that residuals are much more common around 0. With means a better fit.

ggplot(sim1a, aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() 
##

#2.add_predictions() is paired with gather_predictions() and spread_predictions(). How do these three functions differ?
?add_predictions ##A data frame. add_prediction adds a single new column, .pred, to the input dat


?gather_predictions ##spread_predictions adds one column for each model.

?spread_predictions ##gather_predictions adds two columns .model and .pred, and repeats the input rows for each model.

##Answ from docs, but this is useful because it gives the models the analogous tools for wrangling data about the model.

?spread
#3.What does geom_ref_line() do? What package does it come from? Why is displaying a reference line in plots showing residuals useful and important?

?geom_ref_line()
##Comes from modelr package
##It is useful to have a reference line because is a way of visualize how much a residual differs from the ideal, which would be zero (in which case the model captured perfectly the "y")
#4.Why might you want to look at a frequency polygon of absolute residuals? What are the pros and cons compared to looking at the raw residuals?
##Because if the residuals folllow a known distribution it might bring insight about the model, the dataset and the next step in the investigation 
##One of the cons could be that it smoothes the line of residuals, and if positive and negative residuals alternate a lot it might give the impression of something happening there

#23.4.5 Exercises

#1.What happens if you repeat the analysis of sim2 using a model without an 
#intercept. What happens to the model equation? What happens to the predictions?

sim2%>%ggplot(aes(x,y))+geom_point()
mod2w <- lm(y ~ x - 1, data = sim2)
mod2 <- lm(y ~ x , data = sim2)

coef(mod2w)
coef(mod2)
gridw<-sim2%>%
  data_grid(x)%>%
  add_predictions(mod2w)

grid<-sim2%>%
  data_grid(x)%>%
  add_predictions(mod2)

grid
sim2%>%ggplot(aes(x))+
  geom_point(aes(y=y))+
  geom_point(data = gridw, aes(y = pred), color = "red", size = 4)+
  geom_point(data = grid, aes(y = pred), color = "blue", size = 4, alpha = 0.3)
##The model equation changes its coefs and the predictions stay the same.



#2.Use model_matrix() to explore the equations generated for the models I fit to sim3 and sim4. Why is * a good shorthand for interaction?
mod1_sim3<- lm(y ~ x1 + x2, data = sim3)
x3<-model_matrix(sim3, mod1_sim3)
sum(x3[["x1:x2b"]] == (x3[["x1"]] * x3[["x2b"]]))==length(x3$x1)


mod2_sim3<-lm(y ~ x1 * x2, data = sim3)
x3a<-model_matrix(sim3, mod2_sim3)
sum(x3a[["x1:x2b"]] == (x3a[["x1"]] * x3a[["x2b"]]))==length(x3a$x1)

##The asterisk `*` is good shorthand for an interaction since an interaction between `x1` and `x2` includes terms for `x1`, `x2`, and the product of `x1` and `x2`.


#3.Using the basic principles, convert the formulas in the following two models into functions. (Hint: start by converting the categorical variable into 0-1 variables.)

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)


sim3t<-sim3%>%
  mutate(v = 1)%>%
  spread(x2,v)%>%
  arrange(y)
sim3t[is.na(sim3t)]<-0

sim3t_mod<-model_matrix(sim3,mod1)
sim3t<-sim3t%>%
  arrange(x1)%>%
  mutate(Intercept=1)%>%
  select(Intercept,x1,b:d)

all(sim3t$d==sim3t_mod$x1d)


#4.For sim4, which of mod1 and mod2 is better? I think mod2 does a slightly better job at removing patterns, but it’s pretty subtle. Can you come up with a plot to support my claim?

mod1_4 <- lm(y ~ x1 + x2, data = sim4)
mod2_4 <- lm(y ~ x1 * x2, data = sim4)
sim4
library(modelr)

sim4_mods <- sim4 %>% 
  gather_residuals(mod1_4, mod2_4)

ggplot(sim4_mods, aes(x = resid, colour = model)) +
  geom_freqpoly(binwidth = 0.5) +
  geom_rug()

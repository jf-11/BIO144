########################################################################

# BIO144

########################################################################
# WEEK 4 IC EXERCISE 2
########################################################################

rm(list=ls())
library(tidyverse)
library(ggthemes)
library(latex2exp)
library(ggfortify)
library(GGally)
library(gridExtra)

########################################################################

# MILK EXERCISE

########################################################################

milk_data <- read.csv("~/Desktop/BIO144 Übungen/datasets_master/milk_rethinking.csv")
View(milk_data)

########################################################################

# HYPOTHESIS

# higher calories, higher brain mass?
# biological reason?

########################################################################

# VISUALIZE THE DATA WE ARE INTERESTED IN

p1 <- ggplot(milk_data,aes(kcal.per.g)) + geom_histogram(bins=6)
p2 <- ggplot(milk_data,aes(mass)) + geom_histogram(bins=6)
p3 <- ggplot(milk_data,aes(neocortex.perc)) + geom_histogram(bins=6)
grid.arrange(p1,p2,p3)

milk_data %>%
  gather(key=variable, value=value, kcal.per.g, mass, neocortex.perc) %>%
  ggplot() +
  geom_histogram(aes(x = value), bins=6) +
  facet_wrap(~variable, scales = "free", nrow = 3)

########################################################################

# HOW MANY OBESERVATIONS?

nrow(milk_data)

milk_data <- na.omit(milk_data)
nrow(milk_data)

########################################################################

# DATA LOOKS SKEWED, TRY LOG TRANSFORM

milk_data <- milk_data %>% 
  mutate(log10_m = log10(mass),log10_kc = log10(kcal.per.g))

########################################################################

# RELATIONSHIPS?

# kcal.per.g and mass
ggplot(milk_data,aes(mass,kcal.per.g)) + geom_point()
# not much going on

# kcal.per.g and neocortex.perc
ggplot(milk_data,aes(neocortex.perc,kcal.per.g)) + geom_point()
# not much going on 

#mmass and neocortex.perc
ggplot(milk_data,aes(log10_mass,neocortex.perc)) + geom_point()
# positive relationship

########################################################################

# DEGREES OF FREEDOM

nrow(milk_data) - 3

########################################################################

# SET UP THE MODEL

linear_model <- lm(kcal.per.g ~ mass + neocortex.perc,data=milk_data)

# check the model

summary(linear_model)
autoplot(linear_model)
# does not look that good
# perhaps issue with pattern in residuals and qqplot

########################################################################

# slope for mass
# -0.0054

# slope for cortex
# 0.018

# significantly different from zero!
# explanatory power of the model is 27%

########################################################################

# a graph that show the estimated relationship between kcal.per.g and mass, 
# and another of the estimated relationship between kcal.per.g and neocortex.perc.

# leaving both explanatory variables constant
neocortex_constant <- expand.grid(mass=seq(min(milk_data$mass),max(milk_data$mass),length=17),
                                  neocortex.perc=mean(milk_data$neocortex.perc))
mass_constant <- expand.grid(neocortex.perc=seq(min(milk_data$neocortex.perc), max(milk_data$neocortex.perc),length=17),
                             mass=mean(milk_data$mass))
# predict the values
mod1 <- predict(linear_model,newdata = neocortex_constant,interval = 'confidence')
mod2 <- predict(linear_model,newdata = mass_constant, interval = 'confidence')

# create new dataframe with data and predicted
df1 <- cbind(neocortex_constant,mod1)
df2 <- cbind(mass_constant,mod2)
View(df1)

ggplot(data=df1) +
  geom_line(mapping=aes(x=mass,fit)) +
  geom_smooth(mapping=aes(x=mass,fit,ymin=lwr,ymax=upr),stat='identity')

ggplot(data=df2) +
  geom_line(mapping=aes(neocortex.perc,fit)) +
  geom_smooth(mapping=aes(neocortex.perc,fit,ymin=lwr,ymax=upr),stat = 'identity')

# In the first plot we can see a negative relationship betweeen kcal.per.g and 
# mass. When mass decreases one unit, then ckal.per.g increases by 0.54 %. 
# In the second plot we can see a positive relationship between kcal.per.g and
# neocortex.perc. When neurocortex.perc increases by one unit, then kcal.per.g
# increases by 1.75 %.

########################################################################

# The answer is that because the two explanatory variables are correlated 
# and they have opposite signed (one positive, one negative) correlations 
# with the response variable, they tend to cancel each other out.

# The overall lesson here is when explanatory variables are correlated, 
# we must be careful. Very careful.

########################################################################

# ADJUSTED R^2

r1 <- rnorm(17)
test <- lm(kcal.per.g~neocortex.perc + mass + r1, milk_data)
r2 <- rnorm(17)
testt <- lm(kcal.per.g~neocortex.perc + mass + r1 + r2, milk_data)
r3 <- rnorm(17)
testtt <- lm(kcal.per.g~neocortex.perc + mass + r1 + r2, milk_data)

summary(linear_model)
summary(test)
summary(testt)
summary(testtt)

# R^2 increases and adjusted R^2 decreases.

########################################################################

# TESTING WITH QQPLOTS
# qqfunc FUNCTION

qqfunc <- function(model, num.reps) {
  
  N <- length(resid(model))
  sigma <- summary(model)$sigma
  
  x <- rnorm(N, 0, sigma)
  xx <- qqnorm(x, plot.it=F)
  xx$y <- xx$y[order(xx$x)]
  xx$x <- xx$x[order(xx$x)]
  plot(xx$x, scale(xx$y), pch=19, col="#00000011", type="l",
       xlab="Theoretical quantiles",
       ylab="Standardised residuals")
  ##qqline(x)
  
  for(i in 2:num.reps) {
    
    x <- rnorm(N, 0, sigma)
    xx <- qqnorm(x, plot.it=F)
    xx$y <- xx$y[order(xx$x)]
    xx$x <- xx$x[order(xx$x)]
    points(xx$x, scale(xx$y), pch=19, col="#00000011", type="l")
    
  }
  
  xx <- qqnorm(m1$residuals, plot.it=F)
  points(xx$x, scale(xx$y), col="red", pch=19)
}

qqfunc(linear_model,100)
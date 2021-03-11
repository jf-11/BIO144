########################################################################

# BIO144

########################################################################
# WEEK 3 IC EXERCISE 1
########################################################################

rm(list=ls())
library(tidyverse)
library(ggthemes)
library(latex2exp)
library(ggfortify)

########################################################################

# IMPORTING DATA

financinghealthcare <- read.csv("~/Desktop/BIO144 Übungen/datasets_master/financing_healthcare.csv")

# Being sure of the question and data. Here we are interested in if there is a relationship between 
# healthcare spending in a country and child mortality in that country. To simplify the question, 
# let's ask that question for only 2013. I.e. is there a relationship between healthcare spending of a
# country and child mortality in that country in 2013.

########################################################################

# QUESTION 1

# How many countries (rows = number of data points) are available for the question at hand?
# Hint: the answer is not 178. (Do not include the life expectancy variable in the dataset.)


reduced_data <- financinghealthcare %>% 
  filter(!is.na(health_exp_total) & !is.na(child_mort) & year == 2013)
View(reduced_data)

nrow(reduced_data)
# 186

# PLOT THE DATA

reduced_data %>% 
  ggplot() + aes(health_exp_total,child_mort) +
  geom_point()

########################################################################

# QUESTION 2

# PROBLEM? -> DOESNT LOOK LIKE A LINEAR RELATIONSHIP
########################################################################

# QUESTION 3

# TRANSFORMING? -> LOG TRANSFORMATION

transformed_data <- reduced_data %>% 
  mutate(log_health = log10(health_exp_total), log_child = log10(child_mort))

########################################################################

# QUESTION 4

# REPLOT

transformed_data %>% 
  ggplot() + aes(log_health,log_child) +
  geom_point()

# -> LOOKS BETTER

########################################################################

# QUESTION 5

# Its a good idea to do log10 transformation, rather than natural log, in this case, 
# as it makes the values on the axes easier to relate back to the raw data. In this 
# case, when we make a graph of the log10 transformed variables, we get numbers like 
# 0.5, 1, 1.5, 2.0, etc on the axes.
# What value of the raw variable does a log10 transformed value of 2.0 correspond to?
  
# -> 2 is eauivalent to 100 (10^2)

########################################################################

# QUESTION 6

# The second regression preliminary: Its worth making a guess of the intercept 
# and slope of our regression before letting R loose!
# To guess the intercept, it's useful to have a version of the graph in which 
# the x-axis reaches zero. You can make this happen by adding to your ggplot + xlim(0, 5).
# Its also useful to have the y-axis go far enough to include the guessed intercept: + ylim(0, 4).
# With this graph, put a pen (or something straight) on your screen, where you think the best 
# fit regression line will be. Then look where your pen cuts the y-axis. This value is your 
# guessed intercept (on the log transformed scale -- do not back transform to the raw scale).

#What is your guess?

transformed_data %>% 
  ggplot() + aes(log_health,log_child) +
  geom_point() + xlim(0,5) + ylim(0,4)

# intercept: 3.25
# slope: -0.76 (change y)/(change x)

########################################################################

# QUESTION 7

# Prof. Petchey usually thinks about the degrees of freedom for error as how 
# much freedom the data has, given the model. A model with lots of coefficients 
# will allow the data less freedom. To quantify this, we count the number of data 
# points, and subtract the number of coefficients (also known as parameters) in the model we are fitting.

# -> df = 184

########################################################################

# QUESTION 8

# ASSIGNINGING LINEAR MODEL

lin_mod <- lm(log_child~log_health,data=transformed_data)

# lm stands for linear model (there are more lin. models than lin. reg.)

########################################################################

# QUESTION 9

# Match what should go in place of ??X?? in the information we give to the lm() function.

# -> B C A

########################################################################

# Checking your residuals

# 1. Check the residuals seems normally distributed.

# 2. Check for pattern in the residuals by looking at a graph of the residuals versus the fitted values

autoplot(lin_mod)

# QUESTION 10

# What R function do you use to get the residuals of a fitted model?
  
residuals(lin_mod)

########################################################################

# QUESTION 11

# What R function do you use to get the fitted values?
  
fitted(lin_mod)

########################################################################

# QUESTION 11

# Make a histogram of the residuals.
# Do you think we can safely assume that the residuals are normally distributed?

ggplot(lin_mod) + geom_histogram(aes(y=residuals(lin_mod),..density..),bins=30) +
  coord_flip()

########################################################################

# QUESTION 12

# Make a graph of the residuals against the fitted values.
# Do you think there is pattern in how the residuals are related to the 
# fitted values? For example, if this data seemed to follow a curve, we might 
# conclude that the data is curved, and so our linear model is not so well justified.

ggplot(lin_mod) + aes(x=fitted(lin_mod),y=residuals(lin_mod)) +
  geom_point()

# if this data seemed to follow a curve, we might conclude that the data is curved, 
# and so our linear model is not so well justified.
# -> doesnt follow a curve

# There are a few large residuals, but probably nothing to worry about. These are potential 
# outliers, and it might be worth checking if they have a strong effect on the conclusions we 
# make. You will have the chance to do this in following weeks of the course.

########################################################################

# Interpreting the model
# We want to look at a few things.

# The slope.
# The intercept.
# The amount of variability explained.
# The statistical significance.
# The degrees of freedom for error.

summary(lin_mod)

# QUESTION 13

# What is the estimated intercept (of the regression on all 186 data points)?

lin_mod$coefficients

# -> 3.29

########################################################################

# QUESTION 14

# What is the estimated slope (of the regression on all 186 data points)?

lin_mod$coefficients

########################################################################

# A true story... when Prof. Petchey wrote this practical, he did exactly what 
# he's now advising you. Make a guess of the slope from the graph, and then use lm 
# to calculate the slope. However, he found that they were quite different! The guessed 
# slope was about -0.75 but the value given by lm was around -1.0. This was too big a difference 
# to ignore! It was like an alarm bell ringing, that something was probably wrong.

# Prof. Petchey found that he'd got the response and explanatory variable in the lm switched the 
# wrong way round. Fixing this made the calculated slope also about -0.75.

# This is a real example of how making a guess before doing the analysis and comparing this to the 
# calculated value provides us with a method for cross checking our work. If we always build in such 
# cross checks, we will much more frequently spot our mistakes. And if there is one thing that's 
# certain... we will make mistakes!

# QUESTION 15

# What fraction of the variation in log child mortality is explained?

summary(lin_mod) # multiple or adjusted R-squared

########################################################################

# QUESTION 16

# Looking at the p-value of the slope, would you conclude that you can safely 
# reject the null hypothesis that the slope is not different to zero?

summary(lin_mod)
  
# -> p value is < 0.001 so i think its safe to say yes. So there is a linear dependancy.
# So we can very safely reject the null hypothesis that the slope is zero (i.e. the line is horizontal).

########################################################################

# QUESTION 17

# How many degrees of freedom for error does R tell us there are?

summary(lin_mod)

########################################################################

# QUESTION 18

# "On a log-log scale, there was a negative and linear relationships between child 
# mortality and health care spending, with approximately 1 unit of difference in log 
# health care spend corresponding to -1 unit change in log child mortality."

########################################################################

# QUESTION 19

# linear regression, 184 degrees of freedom for error, t = 24.1, p < 0.0001, r-squared=0.76
# We must give degrees of freedom. Here we are reporting the t-statistic of the t-test on the 
# slope that R has given us in the summary information. And the p-value as a less than 0.0001 
#(even though its much much smaller than even this).
#We give the degrees of freedom, the test (t-test), the test statistic, because these will allow 
#someone to check if the degrees of freedom here correspond with the study design, and will allow 
#someone to recalculate and therefore check the stated p-value.

########################################################################

# FINAL CONCLUSION

# On a log-log scale, there was a negative and linear relationships between child 
# mortality and health care spending, with approximately 1 unit of difference in 
# log health care spend corresponding to -1 unit change in log child mortality 
# (linear regression, 184 degrees of freedom for error, t = 24.1, p < 0.0001).

ggplot(transformed_data) + aes(log_health,log_child,color=continent) +
  geom_point() + geom_smooth(method='lm',formula = y~x,color='red') +
  labs(x='log of health care',y='log of child mortality') + 
  annotate(geom='text',x=3.666785,y=0.6232493,label='Switzerland', size= 4)

########################################################################

# QUESTION 20

# Log transformation
# For example, which of these seems true?

# When health care spending is relatively high, say greater that 
# 2000 dollar per capita per year, there is a weak / shallow relationship 
# with child mortality.

########################################################################

# CODE UNTRANFORMED DATA

## make a graph with regression line on the raw axes

ggplot(data=fhc1, aes(x=health_exp_total, y=child_mort)) +
  geom_point() +
  geom_line(aes(y=10^predict(m1)), col="red")

########################################################################

# QUESTION 21

# Which of these is the most important reason that we cannot assume that a 
# change in health care spending will result in a change in child mortality.

# Yes, the variation in the healthcare spend is not an experimentally applied 
# treatment, it is just observed. The relationship we see may or may not be directly 
# causal. We cannot tell from this data.

########################################################################
########################################################################



















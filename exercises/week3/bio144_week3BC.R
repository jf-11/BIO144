########################################################################

# BIO144

########################################################################
# WEEK 3 BC
########################################################################

rm(list=ls())
library(tidyverse)
library(ggthemes)
library(latex2exp)
library(ggfortify)

########################################################################
#5.5 Simple linear regression
# plants grow better with more moisture is assumption
# 
########################################################################

# import data
plant_data <- read.csv("~/Desktop/BIO144 Übungen/datasets_master/plant.growth.rate.csv")

# take a look
glimpse(plant_data)

# make a first plot
plant_data %>% 
  ggplot() +
  aes(soil.moisture.content,plant.growth.rate) +
  geom_point() + 
  labs(y=TeX('Growth Rate $\\frac{mm}{week}$'),x='Soil Moisture')

# pre estimate slope and intercept:
# slope: 15 --> 30 (growth rate varies) / 2 (soil moisture varies)
# intercept: 15-20
# degrees of freedom --> 50 - 2 = 48

# making a simple linear regression 
# we use function lm() for this

linear_model <- lm(plant.growth.rate~soil.moisture.content,data=plant_data)

# now use autoplot function of ggfortify
autoplot(linear_model,smooth.colour = NA) # use NA to surpess line, not really beneficial

# top left
# tells us whether line is appropriate or not, if things gone wrong hump-shapes or valleys will be 
# present, this would mean structure of model is wrong. we dont want to see relationship

#top right
# evaluates if if residuals are normally distributed, dots are residuals and dashed line is expectation
# under normal distribution, much better than making histogram especially with small sample size

# bottom left
# evaluates assumption of equal variance, y axis is standardized indicator of variation, linear models
# assume that the variance is constant over all predicted values of response variable, there should
# be no pattern, there might be one if example variance increases with mean, we dont want to see 
# a relationship

# bottom right
# this evaluates leverage, tool to detect influential data points (ones that moove gradient more than
#expected) and also outliers.

# interpretation
# we will use anova() and summary()
# anova() does not produce an ANOVA which compares means. It produces a SUM-OF-SQUARES TABLE!
# this is a classic table in statistics. It provides the overall F-value (ratio of variance explained by
# the explanatory variables to the leftover variance) and also estimate and adjusted R^2.
# summary() produces table of estimates of the coefficients (intercept and slope)

anova(linear_model)

# F-value is large --> error variance is small relative to variance of explanatory variable
# small p-value --> we would get such a large F-value fewer than a million --> not a result by chance

summary(linear_model)

# slope or gradient is associated with explanatory variable --> on x-axis
# we estimated pretty well with 15 and 15-20

# interpretation
# Soil moisture had a positive effect on plant growth. For each unit increase in soil moisture, plant
# growth rate increased by 12.7 mm/week (slope=12.7, t=12.5, df=48,p<0.001).

# from stats back to figures
plant_data %>% 
  ggplot(aes(soil.moisture.content,plant.growth.rate)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(y=TeX('Growth Rate $\\frac{mm}{week}$'),x='Soil Moisture')

# shown in gray is the standard error of the fit
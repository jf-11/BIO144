########################################################################

# BIO144

########################################################################
# WEEK 4 BC
########################################################################

rm(list=ls())
library(tidyverse)
library(ggthemes)
library(latex2exp)
library(ggfortify)
library(GGally)

########################################################################

# LURKING VARIABELS
# statistics can be missleading...hostpital example from video
# look at health when they arrive -> conditional variable (lurking var)
# smoker or non smokers, but age is also a factor so have to divide
# death penalty -> no raccism, but when victim also considered there is
# AVOID: no clear way, can divide how you want, but be CAREFUL!!!!!!!

########################################################################

# MULTIPLE LINEAR REGRESSION PART 1

# importing dataset
bodyfat_data <- read.delim("~/Desktop/BIO144 Übungen/datasets_master/bodyfat.txt")

# select only few cols
data <- select(bodyfat_data,bodyfat,weight,abdomen)

# some histograms
ggplot(data) + aes(bodyfat) + geom_histogram(bins=30)
ggplot(data) + aes(abdomen) + geom_histogram(bins=30)

# pairsplot
ggpairs(data)

########################################################################

# QUESTION 1

# What dplyr function did we use to make a data set with only the three 
# variables we wanted?

# select

# Say we found that the body fat variable was rather non-normal, and it 
# looked like log transforming it might help. Which dplyr function would we 
# use to add the transformed variable to the dataset?

# mutate

########################################################################

# DEGREES OF FREEDOM

# 1 df away for total model and one for each continous variable
# -> 3 -> 249 df

# EXPLANATION
# we take from total away the parameters we estimate...
# intercept, and a slope for each continous variable...
# -> 3 -> 249 df

########################################################################

# QUESTION 2

# If we had a dataset with 28 observations and we planned to make a 
# multiple regression model with four continuous explanatory variables, 
# how many degrees of freedom for error would we have.

# 23

########################################################################

# ASSIGNING THE MODEL

model <- lm(bodyfat~weight+abdomen,data=data) # makes no diff if weight
# or abdomen first

# diagnostics
autoplot(model)
# qq looks good, res vs. fitted looks also good, scale location also no pattern
# potentially one outlier but we will leave it in.

# look at model
summary(model)
# we get intercept at the two slopes
# seems like weight has negative relationship, standart errors are small
# conclusion: statistically significant relationship, but it was positive
# between weight and bodyfat when only those are considered.
# df is as we expected...

########################################################################

# ANOTHER INTERPRET
# CONFINT

confint(model)
# confint do not include 0, if they do means zero relationship

########################################################################

# WE CANNOT INFER CAUSE AND EFFECT!!!

########################################################################

# PREDICTING

predict(model)
# these values are the predicted values for bodyfat corresponding to weight and abdomen
fitted(model)

########################################################################

# NEW DATA FRAME WITH ABDOMEN CONSTANT

new_data <- expand.grid(weight=seq(min(data$weight),max(data$weight),length=100),
            abdomen=mean(data$abdomen))

View(new_data)

p1 <- predict(model,newdata = new_data,interval = 'confidence')

View(p1)

n1 <- cbind(new_data,p1)

View(n1)

########################################################################

# VISUALIZATION

ggplot(data=n1) +
  geom_line(mapping=aes(x=weight,fit)) +
  geom_smooth(mapping=aes(weight,fit,ymin=lwr,ymax=upr),stat='identity')

# there is negative rel, y predicted bodyfat and weight on x. less than
# zero bodyfat is not possible.


# do same for abdomen on x and with a fixed weight.
# there is positive relationship.

########################################################################

# METHOD
# type of analysis. multiple lin reg, bodyfat as resp and weight and abdomen as two cont. vars.
# data met assumption of lin reg.

# RESULT
# abdomen strong and positive with bf, weight negatively corr.
# table with effect sizes and CI.
# weight and abdomen together explain 72% of the variation in bf

# CONCLUSION
# weight and abodmen are easy to made and are promising indicators. 
# development to not predict negative bodyfat.

########################################################################

# CHECK OUTLIERS

model2 <- lm(bodyfat~weight+abdomen,data=slice(data,-39))
autoplot(model2)
summary(model2)
summary(model)

# very simmilar model, would not change our conclusions.
# we checked for the importance for this individual and it has not that
# much of a difference.

########################################################################













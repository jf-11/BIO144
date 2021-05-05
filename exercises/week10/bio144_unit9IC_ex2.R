###########################################################################
###########################################################################
###########################################################################
###########################################################################

# BIO144

###########################################################################
# UNIT 9 IC EXERCISE 2
###########################################################################

rm(list=ls())
library(ggplot2)
library(dplyr)
library(MASS)
library(readr)
library(MuMIn)
library(ggthemes)
library(latex2exp)
library(ggfortify)
library(GGally)
library(gridExtra)
library(coefplot)
library(venn)

c_theme_bisq <- theme(legend.position="right",
                      text=element_text(size=12,
                                        family="serif")) +
  theme(panel.background = element_rect(fill = "blanchedalmond")) +
  theme(axis.line = element_line(colour = "black"))
c_theme_withe <- theme(legend.position="right",
                       text=element_text(size=12,
                                         family="serif")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.line = element_line(colour = "black"))
c_theme_sky <- theme(legend.position="right",
                     text=element_text(size=12,
                                       family="serif")) +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  theme(axis.line = element_line(colour = "black"))

###########################################################################

# WORKFLOW --> PLOT --> MODEL --> CHECK ASSUMPTIONS --> INTERPRET --> PLOT

###########################################################################

# A POSTERIORI variable selection is suitable when the aim is to develop a 
# predictive model, but is not appropriate when one aims for explanation.
# Only A PRIORI variable selection is appropriate when one aims for 
# explanation.

# MODEL SELECTION BIO144 --> EVERYTHING ABOUT MODEL (TRANSFORMATIONS, 
# DISTRIBUTION ETC.)
# MODEL SELECTION SOMESTIMES --> ONLY SELECTION OF VARIABLES

###########################################################################


# little chance of a causal link here, even though the correlation is very strong.
# "correlation does not imply causation"
#  ("correlation implies an unknown causative structure") from a book

# research has to be scientifically valid and practical
# interpretation is important 
# LOOK BEYOUND EFFECT SIZES AND P-VALUES AND MAKE INFORMED JUDGEMENTS ABOUT
# WHAT YOU SEE!

###########################################################################
###########################################################################
###########################################################################
###########################################################################

# Question --> 

###########################################################################

# DATA 
link <- "https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/bodyfat.txt"
bodyfat <- readr::read_delim(link, delim="\t", escape_double = FALSE, trim_ws = TRUE)

###########################################################################

# RESCALING VARIABLES

# Make a new variable that is weight in kilograms (1 pound = 0.45 kg

bodyfat <- bodyfat %>% 
  mutate(weight_kg = weight*0.45)

# Now make one model of bodyfat with abdomen and weight in pounds 
# (the original weight variable) and another with weight in pounds replaced 
# by weight in kg.

original_model <- lm(bodyfat~weight+abdomen,data=bodyfat)
transformed_model <- lm(bodyfat~weight_kg+abdomen,data=bodyfat)

###########################################################################

# comparing models
summary(original_model)
summary(transformed_model)

# What is the ratio of slopes for weight in the two models? There are two 
# ways to figure this out. One first one involves thinking first, then 
# checking numerically. The second involved doing the sums first, and then 
# thinking. 
# (When you check numerically, divide the smaller slope by the larger)

summary(original_model)$coefficients[2] / summary(transformed_model)$coefficients[2]

# You could have guessed 0.45 because its what we multipled the original 
# weight variable by to get the kg version. So the slope is scaled by the 
# same factor we multipled the variable by. Cool!

# (Just in case you wondered, nonlinear transformations, such as taking the 
# logarithm, are completely different, and can affect our results and conclusions.)

# Relative to the abdomen slope, the weight slope is larger when its found in kg.

# If we want to compare the slopes, and thereby say something about the 
# relative importance among the variables, we can only do so if we rescale 
# the variables. Often this is done by dividing by the standard deviation, 
# so the rescaled variable has a standard deviation of 1. At the same time, 
# it is common to make the mean of the variable equal 0, which changes the intercept.

###########################################################################

# Make two new variables: scaled_abdomen and scaled_weight and use these 
# in a linear model of bodyfat.

bodyfat <- bodyfat %>% 
  mutate(scaled_abdomen=scale(abdomen), scaled_weight=scale(weight))
new_model <- lm(bodyfat~scaled_abdomen+scaled_weight,data=bodyfat)

###########################################################################

# What is the slope (rounded to two decimal places) for the scaled abdomen 
# variable?
# And the scaled weight variable?

summary(new_model)$coefficients

###########################################################################

# Which of these differs between the model with the original variables and 
# the one with scaled variables?

summary(original_model)
summary(new_model)

# Lots more changed than when we only multiplied a variable. The other things 
# changed because we subtracted the mean also... Especially the intercept and
# its standard error and t and p values changed (well, actually the p-value 
# for the slope appears to have not changed, but this is because it is very 
# very small, so the computer can't show us the very small value its change by). 
# Note that the t and p values for the slopes did not changed, and the 
# r-squared did not change. This is all fine and what we wanted to happen!

###########################################################################

# So, for the range of values of the explanatory variables, or put another 
# way, after we equalise the observed variation in weight and abdomen, 
# the effect size for abdomen is nearly two and a half time stronger than 
# the effect size for weight.

# Now we're making statements about relative importance based on effect size. 
# This is quite different to the statements about relative importance we made, 
# in the previous section, based on r-squared. Those statements were more 
# about relative importance for explaining the data.

# Recall that we also made the explanatory variables have a mean of zero. 
# You may have noticed that this changed the value of the intercept, its 
# standard error and its t-value and p-value. And note that the value of 
# the intercept changed from an impossible value (-45.9) to a possible value 
# (19.2). This is all good, and should lead you to realise that the value 
# of the intercept, and its statistical significance (i.e. if it is significantly 
# different from zero) is arbitrary. Why have the intercept at x = 0? We 
# can put it wherever we like! So why not have it somewhere meaningful, 
# like in the middle of the explanatory variables? This is what we're doing 
# when we set the mean of each explanatory variable at zero. Now the value 
# of the intercept is the predicted value of bodyfat for the mean value of 
# the explanatory variables.

# SOME RECOMMEND TO DO ALWAYS A STANDARDIZIATION!!
# --> being able to make meaningful comparisons of effect sizes, and 
# obtaining meaningful intercepts

###########################################################################
# (Note that its probably still good to present visualisation of the data 
# with unscaled explanatory variables, for ease of interpretation.)
###########################################################################

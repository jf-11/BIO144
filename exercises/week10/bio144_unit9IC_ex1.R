###########################################################################
###########################################################################
###########################################################################
###########################################################################

# BIO144

###########################################################################
# UNIT 9 IC EXERCISE 1
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

# QUESTION
# Imagine we've made our multiple regression model, with a few explanatory 
# variables, and it seems to explain variation in the response variable 
# quite well. Now we'd like to be able to say something about the relative 
# importance of the explanatory variables. Is one particularly important and 
# the others not so (though all many be statistically significant p < 0.05)? 
#Are they all about equally important?

# Decomposing R2
# Standardising explanatory variables
# Visualising coefficients 

###########################################################################

# DECOMPOSING R^2
full_model <- lm(bodyfat ~ weight+abdomen, data=bodyfat)
weight_model <- lm(bodyfat ~weight, data=bodyfat)
abdomen_model <- lm(bodyfat ~abdomen, data=bodyfat)

###########################################################################

# What is the explanatory power (unadjusted r-squared) of the model with 
# both variables, only weight, only abdomen?

summary(full_model)$r.squared
summary(weight_model)$r.squared
summary(abdomen_model)$r.squared

###########################################################################

# Is the sum of the r-squared of the two models with only one explanatory 
# variable greater or less than the one with both variables?

summary(weight_model)$r.squared+summary(abdomen_model)$r.squared

###########################################################################

# What part of the r-squared for the model with both variables 
# (i.e. what part of 0.72) is definitely unique to abdomen variable?

summary(full_model)$r.squared-summary(weight_model)$r.squared
summary(full_model)$r.squared-summary(abdomen_model)$r.squared

###########################################################################

# And now a perhaps slightly tricky question... what amount of explanatory 
# power is shared by the two variables?

together <- summary(weight_model)$r.squared+summary(abdomen_model)$r.squared
shared <- together - summary(full_model)$r.squared
shared

###########################################################################

# If you only had time to measure one of weight or abdomen, in order to 
# predict bodyfat, which would you measure?

# I'd choose abdomen to measure, if I could measure only one, because it 
# has the greater explanatory power, and by far the greatest unique 
# explanatory power. In that sense, one can say abdomen has greater 
# importance than weight.

###########################################################################

# What is we have three or more explanatory variables?

venn(3,snames='A,B,C',linetype='dashed', ggplot=TRUE,zcolor='red,blue,green')

# we can do the same with decomposing R^2.

###########################################################################

# we see shared R^2 only when we put both variables into the model.
# Shared explanatory power is caused by correlation among explanatory variables. 
# The greater the correlation, the more is shared.
# if variables are perfectly correlated, i.e. one is a linear 
# transformation of another --> whole R^2 is shared...

# As always, correlation (also called collinearity) among explanatory variables 
# presents us with considerable challenges. We can avoid it by thinking hard about 
# the question and data we will collect to answer it BEFORE we start collecting it.

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

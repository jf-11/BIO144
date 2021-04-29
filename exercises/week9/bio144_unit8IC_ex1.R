###########################################################################
###########################################################################
###########################################################################
###########################################################################

# BIO144

###########################################################################
# UNIT 8 IC EXERCISE 1
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
###########################################################################
###########################################################################
###########################################################################

# Question --> How to PREDICT age of abalone

###########################################################################

# DATA 
# 400 individual abalone, TRUE age, sex and other physical measures
dd<- read.csv("~/Desktop/BIO144 Übungen/datasets_master/abalone_age.csv")

###########################################################################

# Distribution of response variable Rings?
ggplot(dd) +
  aes(Rings) +
  geom_histogram(bins=30) +
  c_theme_sky

# its not very normal, rather sharp peak, maybe bit skewed...
# integer (whole numbers only) variable should make us think twice about 
# modelling this with a linear model

###########################################################################

# Which variable is least important?
m1 <- lm(Rings ~ ., data = dd)
AIC(m1)

dropterm(m1, sorted = TRUE)
dt1 <- update(m1, . ~ . -Length_mm)
AIC(dt1)

# Length_mm AIC of 588.78 instead of 590.76

###########################################################################

# What is the change in adjusted r-squared caused by removing the variable 
# that was the answer to the previous question? Round to two decimal places.

summary(m1)
summary(dt1)
0.5083 - 0.507
# 0 --> The very small postive of change in adjusted r-squared is consistent 
# the small change in AIC caused by removal of the variable.

###########################################################################

# Keep removing variables from the model one at a time, choosing to remove 
# the one that increases the performance of the model the most, or decreases 
# it the least. Do this until no more variables can be removed without 
# causing a change in AIC of more than 4 units. Which variables remain in 
# the model?

AICc(dt1)
dropterm(dt1, sorted=TRUE) # remoove Shel_weight_g
dt2 <- update(dt1, . ~ . -Shell_weight_g)
AICc(dt2)-AICc(dt1) # decrease of -2

AICc(dt2)
dropterm(dt2, sorted=TRUE) # remoove Height_mm
dt3 <- update(dt2, . ~ . -Height_mm)
AICc(dt3)-AICc(dt2) # increase of 0.11

AICc(dt3)
dropterm(dt3, sorted=TRUE) # remoove Sex
dt4 <- update(dt3, . ~ . -Sex)
AICc(dt4)-AICc(dt3) # increase of 4.9 so we stop here and use dt3

# We take dt3:
model.sel(dt3)
summary(dt3)

# The final model has AIC of 587.05. If we remove the 
# Sex variable we cause the smallest decrease in performance, 
# but still AIC increases to 592.13 units, and increase of 5.08 units
# you can just look at dropterm of dt1 and then you see the change of the
# AIC

###########################################################################

# se the stepAIC function to do automatic model selection. 
# Do "forward", "backward", and "both"

m0 <- lm(Rings ~ 1, dd) # simplest model
m1 <- m1 # full model

step_forward <- stepAIC(m0,direction='forward', 
                        scope=list(upper=m1, lower=m0))
step_backward <- stepAIC(m1,direction='backward', 
                        scope=list(upper=m1, lower=m0))
step_both <- stepAIC(m1,direction='both', 
                        scope=list(upper=m1, lower=m0))

# Which two selection methods result in the same and best model?
models <- list(sf = step_forward, sba = step_backward, sbo = step_both)
model.sel(models)

# backward and both

# What is the difference (rounded to two decimal places) in AICc between 
# the model you selected manually for previous questions, and the model 
# selected by backward stepAIC?

AICc(dt3)-AICc(step_backward)

# 0.12

# The difference in the model selected is because we removed the Height_mm 
# variable, but stepAIC did not. We did so because it didn't cause a 
# particularly large increase in AIC, whereas the stepAIC function will not 
# remove a variable if it increases AIC by any amount. I.e. it has a more 
# inclusive stopping rule.

###########################################################################

# final method: use the function dredge, without interaction terms
# AICc as criteria
# get 10 best models

s1 <- dredge(m1)
best_ten_models <- get.models(s1, subset=c(1:10)) # best 10
model.sel(best_ten_models)
# models are ranked by AICc so we dont have to specify this. and they are
# already sorted so we can just use the vector above for the first ten.

###########################################################################

# Of the models with delta AIC of less than 5, 
# how many does the Diameter_mm variable occur in?

model.sel(get.models(s1, subset=delta<5))

# 9

###########################################################################

# Think about how you would report the findings. There isn't a perfect way, 
# but again, one could choose to focus on the statistics or the practical 
# implications. The latter is usually preferable, unless you're target 
# audience is statisticians.

# Also think about what you would do next, in order to validate that the 
# model really does what its supposed to.

final_model <- get.models(s1, subset = delta==0)[[1]]
anova(final_model)

library(patchwork)
p1 <- ggplot(mapping=aes(x=dd$Rings, y = fitted(dt3))) +
  geom_point() +
  geom_abline(intercept = 0, slope=1) +
  coord_fixed(ratio = 1) +
  xlab("Observed age\n(number of growth rings)") +
  ylab("Predicted age\n(number of growth rings)") +
  c_theme_sky
p2 <- ggplot(mapping = aes(x = dd$Rings - fitted(dt3))) +
  geom_histogram() +
  xlab("Error in predicted age")
p1 + p2 + plot_annotation(tag_levels = 'A') + c_theme_sky

###########################################################################
###########################################################################
###########################################################################
###########################################################################

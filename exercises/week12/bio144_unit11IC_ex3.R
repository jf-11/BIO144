###########################################################################
###########################################################################
###########################################################################
###########################################################################

# BIO144

###########################################################################
# UNIT 11 IC EXERCISE 2
###########################################################################

rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyr)
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
library(ggiraph)
library(ggiraphExtra)
library(broom)
library(boot)
library(janitor)
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

# OVERDISPERSION describes the observation that variation is higher than 
# would be expected.

# DEVIANCE is a measure of error; lower deviance means better fit to data.
# saturated model, proposed model, null model 
# NullDeviance=2(ll(SaturatedModel)−ll(NullModel))
# ResidualDeviance=2(ll(SaturatedModel)−ll(ProposedModel))

# TRANSFORMED OR GLM WITH LINK FUNCTION
# LINK FUNCTION --> log(E(y)) = ...
# TRANSFORMED ----> E(y_log) = ...
# log von E von y VS. E von logarithmiertem y

###########################################################################

# Count data 
# uses logit as link function
# # response can take two values --> 0 or 1...

###########################################################################
###########################################################################
###########################################################################

# QUESTION / TASK
# predict death based on information derived from echocardiograms
# response --> alive_at_1


###########################################################################

# DATA 
dd <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/echocardiogram.data.csv",
         col_names=T, na="?")

###########################################################################

# make a list of the echocardiogram features in the dataset
# see webpage

###########################################################################

## check one of the relationships
ggplot(dd, aes(x=lvdd, y=alive_at_1)) +
  geom_point(position=position_jitter(height=0.1, width=0),
             alpha=0.5) +
  stat_smooth(method="glm", method.args = list(family = "binomial"))

## and another
ggplot(dd, aes(x=fractional_shortening, y=alive_at_1)) +
  geom_point(position=position_jitter(height=0.1, width=0),
             alpha=0.5) +
  stat_smooth(method="glm", method.args = list(family = "binomial"))

## and lets use dredge to get the models that best predict alive_at_1
## first make data without nas
dd_no_nas <- na.omit(dd)
## Then specify the full model
full_mod <- glm(alive_at_1 ~ pericardial_effusion +
                  fractional_shortening +
                  epss +
                  lvdd +
                  wall_motion_index,
                data = dd_no_nas,
                family = "binomial",
                na.action = "na.fail")
## there we have said that if there are any NAs, please create and error and stop
## we already checked there are no NAs, but this double safety check is still very wise

## do the dredge
dredge_out <- dredge(full_mod)

## get and look at the best model
best_dredge <- get.models(dredge_out, subset = delta==0)[[1]]
anova(best_dredge, test = "LRT")
## the model with lowest AIC has only wall motion score as a variable

## look at the models that are with five AIC units of the best
s1_subset <- get.models(dredge_out, subset = delta < 5)
model.sel(s1_subset)
## diameter is included in 9 of these models.
## I think dt3 is the second best! and it has one less parameter

## there are no other variables, so we don't need to hold them
## constant when making a graph
ggplot(dd, aes(x=wall_motion_index, y=alive_at_1)) +
  geom_point(position=position_jitter(height=0.1, width=0),
             alpha=0.5) +
  stat_smooth(method="glm", method.args = list(family = "binomial"))
## not really so impressive!

## and I leave the results sentence up to you... post your ideas in the
## Discussion Forum
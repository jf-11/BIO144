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
# binomial models --> explain species habitat requierements. one looks
# to see which of a set of environmental variables explains the presence 
# or absence of a species in a particular location...

# provide explanations of the presence or absence of the White Tailed
# Ptarmigan

###########################################################################

# DATA 
data <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/presabs_both_IndYears_allvars_final.csv")

###########################################################################

# list of environmental factors
environmental_factors <- list('BCAIbX','BCAlbY','BEC','MH','AT','CWH',
                              'Elevation','Aspect','Slope','CTI','Rugg9',
                              'Tave_sm','MSP','PAS')

###########################################################################

# look on wikipedia and formulate a hypothesis
# alpine species
# above or near timber line
# Alaska, Canada, NewMexico

# --> BEC, Elevation, Rugg9, Tave_sm, MSP

###########################################################################

# do some graphs

data <- data %>% mutate(present=as.factor(pres))

# check if Alpine tundra important
ggplot(data) + aes(pres,fill=BEC) + geom_histogram(bins=5) + facet_grid(~BEC)
# looks like alpine tundra is important

# chekc for Elevation
ggplot(data) + aes(present,Elevation,color=present) + geom_boxplot() + facet_grid(~present)
# seems like more likely present in higher elevation
ggplot(data, aes(x=Elevation, y=pres)) +
  geom_point(position=position_jitter(height=0.1, width=0),
             alpha=0.5) +
  stat_smooth(method="glm", method.args = list(family = "binomial"))
## yes, there is greater presence at higher elevation

## and do the same for aspect
ggplot(data, aes(x=Aspect, y=pres)) +
  geom_point(position=position_jitter(height=0.1, width=0),
             alpha=0.5) +
  stat_smooth(method="glm", method.args = list(family = "binomial"))
## perhaps a weak relationship with aspect

## we can check if this relationship is supported with a binomial model:
m1 <- glm(pres ~ Elevation + Aspect, data=data, family=binomial)
## the autoplot is not so informative
autoplot(m1)
anova(m1, test = "LRT")
## with a likelihood ratio test, we find very strong evidence
## for both variables

# check for Rugg9
ggplot(data) + aes(present,Rugg9,color=present) + geom_boxplot() + facet_grid(~present)
# doesnt seem to be that important

# check for Tave_sm
ggplot(data) + aes(present,Tave_sm,color=present) + geom_boxplot() + facet_grid(~present)
# looks like there is a pattern

# check for MSP
ggplot(data) + aes(pres,MSP,color=present) + geom_boxplot() + facet_grid(~present)
# seems not to be important

###########################################################################

# correlations among environmental factors...

###########################################################################

## we can also make of one relationship
## while we hold the other explanatory variable (as we did before in the course)
new <- expand.grid(Elevation = seq(min(data$Elevation),
                                   max(data$Elevation),
                                   length = 500),
                   Aspect = mean(data$Aspect))

new2 <- predict (m1, newdata = new, se.fit = TRUE, type = 'response')
together <- cbind (new, new2)

ggplot (data, aes (x = Elevation, y = pres)) +
  geom_point (position=position_jitter(height=0.05, width=0), alpha = 0.5) +
  geom_smooth (data = together,
               mapping = aes (x = Elevation, y = fit,
                              ymin=fit-se.fit, ymax=fit+se.fit),
               stat = "identity")

###########################################################################
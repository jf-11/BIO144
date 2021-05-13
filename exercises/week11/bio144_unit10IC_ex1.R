###########################################################################
###########################################################################
###########################################################################
###########################################################################

# BIO144

###########################################################################
# UNIT 10 IC EXERCISE 1
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
library(pcsl)
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
###########################################################################
###########################################################################
###########################################################################

# DATA 
abalone <- read.csv("~/Desktop/BIO144 Übungen/datasets_master/abalone_age.csv")

###########################################################################

ggplot(abalone) + aes(Rings) + geom_histogram(bins=20) + c_theme_sky

###########################################################################

# we fit the full model
full.model <- glm(Rings ~.,family = poisson(link='log'), data=abalone)

###########################################################################

# Which if any of the variables that were kept in the lm were *not* kept 
# in the glm with poisson family?
dropterm(full.model,sorted = T)
updated.model1 <- update(full.model, . ~ . -Length_mm)
dropterm(updated.model1,sorted=T)
updated.model2 <- update(updated.model1, . ~ . -Shell_weight_g)
dropterm(updated.model2,sorted=T)
updated.model3 <- update(updated.model2, . ~ . -Height_mm)
dropterm(updated.model3,sorted=T)
updated.model4 <- update(updated.model3, . ~ . -Sex)
dropterm(updated.model4,sorted=T)
updated.model5 <- update(updated.model4, . ~ . -Viscera_weight_g)
AIC(full.model)
AIC(updated.model1)
AIC(updated.model2)
AIC(updated.model3)
AIC(updated.model4)
AIC(updated.model5)

# we take updated.model5
summary(updated.model5)

###########################################################################

# what is the dispersion of the new model?
# dispersion = residual deviance / df
164.40/396

###########################################################################

# we change the model family = quasipoisson
new.model <- glm(Rings ~ Diameter_mm + Whole_weight_g + Shuck_weight_g, 
                 family = quasipoisson,data=abalone)

summary(new.model)

###########################################################################

# How similar are the predictions of the lm and the poisson glm.
# Your challenge then, is to make a graph of the predictions of the best lm 
# versus the predictions of the best poisson glm. Little practical 
# significance would be indicated if the predictions were very similar 
# (i.e. lie on the 1:1 line).

lm.model <- lm(Rings ~Sex + Diameter_mm + Whole_weight_g + Shuck_weight_g + Viscera_weight_g,data=abalone)

ggplot(mapping=aes(x=fitted(new.model), y=fitted(lm.model))) + geom_point(alpha=0.5) +
  xlab("Predictions of glm") +
  ylab("Predictions of lm") +
  geom_abline(intercept=0, slope=1, color='red')
cor(cbind(x=fitted(new.model), y=fitted(lm.model)))
## very strong correlation
###########################################################################
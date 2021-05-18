###########################################################################
###########################################################################
###########################################################################
###########################################################################

# BIO144

###########################################################################
# UNIT 11 IC EXERCISE 1
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
# eagle owls --> examination of the pellets containing undigested remains
# of prey. diets of male and female were studied by examination of pellets

###########################################################################

# DATA 
data.eagles <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/EAGLES.CSV")

###########################################################################

# make bar chart that shows the number of each prey type found, for each sex
tt <- xtabs(Count ~ Prey + Sex, data = data.eagles)

## and we can make a plot of the data
ggplot(data.eagles, aes(x=Sex, y=Count, fill = Prey)) +
  geom_col(position="dodge") + c_theme_sky

# is there different pattern among prey for males and females?
# it looks like there is a different pattern, Bird, Hare etc..

###########################################################################

# Chi Square Value?
x <- data.eagles %>% 
  filter(Sex=='Female')
y <- data.eagles %>% 
  filter(Sex=='Male')
dat <- cbind(x$Count,y$Count)
chisq.test(dat)

###########################################################################

# sentence to communicate findings:
# "Patterns of prey usage differed moderately between males and females 
# (X^2 = 12.6, df = 5, p = 0.027); for example females consumed many waterbirds, 
# whereas males consumed many voles."

###########################################################################



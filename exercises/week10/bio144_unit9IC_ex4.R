###########################################################################
###########################################################################
###########################################################################
###########################################################################

# BIO144

###########################################################################
# UNIT 9 IC EXERCISE 4
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
the_URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQFgYX1QhF9-UXep22XmPow1ZK5nbFHix9nkQIa0DzqUhPtZRxH1HtY-hsno32zDiuIHiLb2Hvphk1L/pub?gid=1188775314&single=true&output=csv"
class_RTs <- read_csv(the_URL)

###########################################################################

# individual, sex, and reaction time

# 1 Rename the variables, so they're a bit nicer than in the googlesheet.
# Make a dataset containing only the necessary variables.
# Gather the information that is spread across variables into one variable.

# 1
names(class_RTs) <- c("time", "id", "sex", "first",
                      "verbal", "number", "visual",
                      "weight", "hand", "nonpreferd",  "second",
                      "third",  "fourth", "fifth", "average", "randomnum")

# kicking out variables we dont need
class_RTs <- class_RTs %>% 
  dplyr::select(c(sex,id,first,second,third,fourth,fifth))
View(class_RTs)

###########################################################################

# put the times into average
new_data <- class_RTs %>% 
  pivot_longer(cols = c('first','second','third','fourth','fifth'),
               names_to = 'reactiontime', values_to = 'time')
View(new_data)
nrow(new_data) # same as in solutions

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
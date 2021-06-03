###########################################################################

# BIO144 BC UNIT 12

###########################################################################
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
library(ggiraph)
library(ggiraphExtra)
library(broom)
library(boot)

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

# WORLD OF STATISTICAL TESTS

###########################################################################

# RESPONSE AND EXPLANATORY VARIABLE
# y -> response variable, dependent
# x -> explanatory variable, independent, covariate
# y depends on x
# not all graphs correspond to this graphing setup

# think of test according to types of variables:
# RESPONSE
# CONTINOUS (normally) it's unbounded --> plus and minus infinity and
# it is also symetrical, can also be COUNT data (not continous, discrete values)
# BINARY data (1 or 0 success or not) also discrete, cannot take fractions
# negative numbers also not possible for discrete datas, they are bounded!!
# binary is often transformed into proportions
# COUNT we often use poisson and BINARY we often use binomial...
# there are other response variables...

###########################################################################

# EXPLANATORY 
# two types of explanatory variables
# CATEGORICAL or CONTINOUS
# factor, discrete vs. numeric, numbers
# numeric can also be discrete... but is not categorical then
# NOW WE CAN CHOOSE WHICH TESTS WE WANT TO DO!

###########################################################################

# SCREENSHOT TAKEN

###########################################################################
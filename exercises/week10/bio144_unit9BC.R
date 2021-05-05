###########################################################################

# BIO144 BC UNIT 9

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

# little chance of a causal link here, even though the correlation is very strong.
# "correlation does not imply causation"
#  ("correlation implies an unknown causative structure") from a book

###########################################################################

# EFFECT SIZES AND THE INTERPRETATION OF RESULTS
# practical vs. statistical significance
# statistical significance does differs from significance in real world
# language. 
# statistical significance --> unlikely to be result of chance
# practically significance --> meaningful in real world
# non-significant t-test --> we cannot rule out chance as possible explanation.
# --> more information is needed! Does not mean it is practically unimportant.

###########################################################################

# the concept of effect sizes
# practical significance --> size of effect
# statistical significance --> precision of the estimate
# NO CONCLUSIONS can be drawn from statistical significance to practical significance!
# scientist --> 1) conduct rigorous research leading to reporting of precise
# effect size estimates in language that facilitates interpretation by others.
# 2) to interpret practical significance or meaning of research result.

###########################################################################

# INTERPRETING EFFECTS
# effects means different things to different people
# arbitrary scales --> no obvious connection between score and individuals
# actual state --> problematic to interpret --> well known metrics needed
# scale must be known to be able to deal with the effect size.

###########################################################################

# Cs OF INTERPRETATION --> CONTEXT, CONTRIBUTION, COHEN
# in the right CONTEXT even small effects may be meaningful
# ex: accumulation, probab for large outcome changed, 
# can trigger bigger outcomes, can lead to technolog breaktrough
# Does the observed effect differ from what others have found? (CONTRIBUTION)
# if yes... why?
# effects means different things at different points of time
# find alternative plausible explanations (APEs)
# COHENs controversial criteria
# see learning objective for cohen
# DANGER --> ALSO MINDLESS INTERPRET THESE NUMBERS

###########################################################################

# SUMMARY
# research has to be scientifically valid and practical
# interpretation is important 
# LOOK BEYOUND EFFECT SIZES AND P-VALUES AND MAKE INFORMED JUDGEMENTS ABOUT
# WHAT YOU SEE!

###########################################################################




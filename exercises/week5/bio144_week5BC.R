########################################################################

# BIO144

########################################################################
########################################################################

rm(list=ls())
library(tidyverse)
library(ggthemes)
library(latex2exp)
library(ggfortify)
library(GGally)
library(gridExtra)

library(thematic)
thematic_on(
  bg = "#222222", fg = "white", accent = "bisque",
  font = font_spec("serif", scale = 1.25)
)
library(RColorBrewer)
thematic_on(
  bg = "white", fg = "black", accent = "cyan", qualitative = RColorBrewer::brewer.pal(8,'Set1'),
  font = font_spec("serif", scale = 1.25)
)

########################################################################

# LOAD THE DATA
data <- read.csv('https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/earthworm.csv')

# HAVE A LOOK AT THE DATASET
str(data)
table(data$Gattung)

# magenumfang, gewicht --> cont
# gattung is not cont

########################################################################

# LOOKING AT HISTOGRAMS
ggplot(data) + aes(x=Gewicht) + geom_histogram(bins=30)

# TRANSFORM looks less skewed
data <- mutate(data,log10_Gewicht=log10(Gewicht))
ggplot(data) + aes(x=log10_Gewicht) + geom_histogram(bins=20)

# looks pretty normally distributed no transf needed
ggplot(data,aes(x=Magenumf)) + geom_histogram(bins=20)

########################################################################

# looks curvy, not really an equal spread
ggplot(data,aes(Magenumf,Gewicht)) + geom_point()

# looks much better
ggplot(data,aes(Magenumf,log10_Gewicht)) + geom_point()

# THINK ABOUT SLOPE
# slope --> guess 0.31
# df --> 143 - 2 = 141

########################################################################

# LINEAR REGRESSION

m1 <- lm(log10_Gewicht~Magenumf,data)
autoplot(m1)
summary(m1)

########################################################################

ggplot(data,aes(Magenumf,Gewicht)) + geom_point() +
  labs(x='Gut circumference [mm]', y=TeX('Log_{10} Worm weight [g]'),title = 'Worm Analysis') +
  geom_smooth(method = 'lm',formula = y ~ x,color='cyan')

########################################################################

# look at data

ggplot(data,aes(Gattung,log10_Gewicht, fill = Gattung)) +
  geom_boxplot() +
  geom_point() +
  labs(y='Gewicht')

# means guessing
# L = 0.4
# N = -0.2
# Oc = -0.3

########################################################################

# second model using categorical variable (Gattung)
# Intercept is for reference. (L)
m2 <- lm(log10_Gewicht ~ Gattung,data)
autoplot(m2)
qqfunc(m2,100)

summary(m2)
# note that the estimate for N and Oc is the difference to the refernce L.

anova(m2)

########################################################################

# looking at both explanatory variables (Gattung and magenumf)

ggplot(data, aes(Magenumf,log10_Gewicht,color=Gattung)) +
  geom_point()
# we can see that there seems to be difference in Gattung. 


# generate the third model m3 (2 expl. vars)

m3 <- lm(log10_Gewicht ~ Magenumf + Gattung,data)
# before we fitted all data to have same slope and intercept, one slope
# one intercept
# but now we have 3 intercepts and 1 slope
# 4 params

summary(m3)
autoplot(m3)
# intercept is reference L and the others are the difference...
# df are 139 bc of 4 params estimated...
# we can see that the slope is sifnificant...
# lower intercept for N

########################################################################

# one more model we can do 
m4 <- lm(log10_Gewicht ~ Magenumf * Gattung,data)
# this is now a 3 inter and 3 slope model...
# does the relationship of gewicht and size of small gut thing differ depending
# on gattung --> this is the question now
autoplot(m4)
summary(m4)
# not much higher adjusted R^2
# magenumf is slope for L
# then differences of intercept
# the understen zwei are differences of slopes... --> these have small
# estimates and low stat signif --> lack of evi that different slope
# not stat signifi different from zero --> no diff --> R^2 has not increased
# not explanatory power gained...

########################################################################

# Make a nice graph

ggplot(data, aes(Magenumf,log10_Gewicht,color=Gattung)) +
  geom_point() + geom_smooth(method = 'lm', formula = y ~ x)

########################################################################

# + --> only main effects and * --> with interaction...

########################################################################
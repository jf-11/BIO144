###########################################################################

# BIO144 BC WEEK 7
# BOOK SECTION 6.3 IN PETCHEY CONTAINS NEARLY ALL CONCEPTS DISCUSSED IN
# WEEK 1-7

###########################################################################
###########################################################################

rm(list=ls())
library(tidyverse)
library(ggthemes)
library(latex2exp)
library(ggfortify)
library(GGally)
library(gridExtra)

custom_theme <- theme(legend.position="right",
                      text=element_text(size=12,
                                        family="serif")) +
  theme(panel.background = element_rect(fill = "bisque1")) +
  theme(axis.line = element_line(colour = "black"))

###########################################################################
# ANCOVA
# contains catgorical and continous explanatory variables
# one way ANOVA and REGRESSION combined!
###########################################################################
# load the dataset
limpet <- read.csv("~/Desktop/BIO144 Übungen/datasets-master 3/limpet.csv")
# egg production by limpets with four density conditions in two seasons
# response is eggs and desity is continous and season is categorical
# density -> number of mums sharing food

# does desity dependance differ between sprng and summer?
#######################################################################
# start with a graph
ggplot(limpet) +
  aes(DENSITY,EGGS,color=SEASON) +
  geom_point() +
  scale_color_manual(values=c(spring='magenta',summer='skyblue')) +
  custom_theme
# -> decline in egg production with increasing density
# -> some tendency that production higher in spring
# -> idea for a model -> slope is negative because egg production
# is decreasing with an increasing density... intercept is different
# for the two seasons...
#######################################################################
# assigning the model
limp.mod <- lm(EGGS ~ DENSITY * SEASON, data = limpet)
names(limp.mod)
# diagnostics
autoplot(limp.mod,smooth.colour = NA) +
  custom_theme
# looks great
#######################################################################
# INTERPRETATION
# ANOVA table
anova(limp.mod)
# mean sq -> units of variation that is explained by term...
# we tested the hypothesis that the effect of density on egg production
# in limpets depends on the seqson in which they are reproducing. We
# found no evidence for an interaction between density and season
# (F=0.0711, df = 1,20, p-value = 0.79) indicating that the effects of
# desity and season are additive.
#######################################################################
summary(limp.mod)
#######################################################################
# producing a figure
coef(limp.mod)
# now we use the functions expand.grid() and predict()
# make some new DENSITY values at which we request predictions
new.x <- expand.grid(DENSITY=seq(from=8, to=45, length.out = 10))
head(new.x) # so we got our 'new' densities here
# now we add season to the grid
new.x <- expand.grid(
  DENSITY=seq(from=8, to=45, length.out = 10),
                     SEASON = levels(limpet$SEASON))
new.x #we added the season to the grid
# we have each value assigned with both of the seasons
# using the predict function
# we give three arguments -> a model, newdata and request for ci
new.y <- predict(limp.mod,newdata = new.x, interval = 'confidence')
new.y # now we have the y values for the newdata we generated with
# the expand grid function
#######################################################################
# housekeeping to bring new.x and new.y together
# we rename fit to match the orignal data...
add.these <- data.frame(new.x,new.y)
add.these <- rename(add.these,EGGS=fit)
head(add.these)
View(add.these)
#######################################################################
# FINAL PLOT
ggplot(limpet, aes(DENSITY, EGGS, color=SEASON)) +
  geom_point() +
  # geom_smooth inherits Density and Eggs, bc same name else
  # must be speciefied...
  geom_smooth(data=add.these,
              aes(ymin=lwr, ymax=upr,
                  fill = SEASON), 
              stat = 'identity') + 
  # stat identity is just that nothing else is computed...
  scale_colour_manual(values=c(spring='magenta', summer = 'skyblue')) +
  scale_fill_manual(values=c(spring='magenta', summer = 'skyblue')) +
  labs(title='ANCOVA LIMPETS', caption='FINAL PLOT', 
       subtitle = 'AN ANCOVA ANALYSIS OF LIMPETS AND THEIR EGG PRODUCTION') +
  theme(legend.position="right",
        text=element_text(size=12,
                          family="serif")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.line = element_line(colour = "black"))
#######################################################################
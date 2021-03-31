###########################################################################

# BIO144

###########################################################################
###########################################################################

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

###########################################################################

# Section 5.6 Petchey Book
# Question
# water flea growth rates. Do parasites alter growth rates? 
# Does each of the parasites reduce growth compared with a control non 
# parasites group? 

###########################################################################

# load the data
daphnia <- read.csv("~/Desktop/BIO144 Übungen/datasets_master/Daphniagrowth.csv")
glimpse(daphnia)

########################################################################

# plots
ggplot(daphnia,aes(parasite,growth.rate)) +
  geom_boxplot() +
  coord_flip()

# we can already inteprete this results --> they alter growth rate
# they all reduce compared with the control group...

########################################################################

# construct the anova
model <- lm(growth.rate ~ parasite, daphnia)

########################################################################

# checking the assumptions
autoplot(model)
# looks good, qq plot is also in the range of the possible...

# estimate df


########################################################################

# looking at anova
anova(model)
# null hypothesis of one way anova --> all the groups come from populations
# with same mean.

########################################################################

# treatment contrasts
summary(model)
# control is the reference --> this makes sense...
# treatment contrasts do report the differences between the reference
# level (luckily this is control here) the differences are all negative

# get mean growth rates
means <- daphnia %>% 
  group_by(parasite) %>% 
  summarise(mean.growth.rate = mean(growth.rate))
means

########################################################################

ggplot(daphnia,aes(parasite,growth.rate,color=parasite)) +
  geom_point() +
  geom_point(data=means,aes(parasite,mean.growth.rate, color = parasite), 
             shape = 17, 
             size = 6, 
             alpha = 0.7) +
  coord_flip()

########################################################################

# Section 6.1 and 6.2 Petchey Book
# two way anova
# WORKFLOW --> PLOT --> MODEL --> CHECK ASSUMPTIONS --> INTERPRET --> PLOT

########################################################################

# import dataset
growth <- read.csv("~/Desktop/BIO144 Übungen/datasets_master/growth.csv")

########################################################################

# levels function
levels(growth$diet)
levels(growth$supplement)

#######################################################################

# relevel function
growth <- 
  mutate(growth,supplement=relevel(supplement, ref='control'))

#######################################################################

# lets make a plot
# first get the means
means <- growth %>% 
  group_by(diet,supplement) %>% 
  summarise(meanGrow = mean(gain))

ggplot(means,aes(supplement,meanGrow,color=diet, group=diet)) +
  geom_point() +
  geom_line()

#######################################################################

mod <- lm(gain ~ diet * supplement,growth)
autoplot(mod)
anova(mod)
# look at the interaction term --> low evidence of interaciton
# we tested formally  a hypothesis about wether the effect of supplements
# on cow weight gain depends on the diet...

# summary
summary(mod)
# intercept is reference point for table (barley-control)
# row two and three --> difference for oat-control and wheat-control
# use packages like contrast, rms and multcomp for specifying custom contrasts

#######################################################################

means <- growth %>% 
  group_by(diet,supplement) %>% 
  summarise(meanGrow = mean(gain),
            seGrow = sd(gain)/sqrt(n()))
# n() does counts rows in each group bc has to divide by n

#######################################################################

ggplot(means,aes(supplement,meanGrow,color=diet, group=diet)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=meanGrow - seGrow,ymax = meanGrow + seGrow),width=0.1)

#######################################################################

########################################################################

# BIO144

###########################################################################
# WEEK 6 IC EXERCISE 1
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
  bg = "white", fg = "black", accent = "#0CE3AC",
  qualitative = RColorBrewer::brewer.pal(8,'Set1'),
  font = font_spec("serif", scale = 1.25)
)

###########################################################################

# WORKFLOW --> PLOT --> MODEL --> CHECK ASSUMPTIONS --> INTERPRET --> PLOT

###########################################################################

# QUESTION

###########################################################################

# load data
data <- read.csv('https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/intertidalalgae.csv')
glimpse(data)

###########################################################################

# how many df? lm with two exp vars and their interaction
# --> 64 - 4 (4 treatment combinations, four means to estimate (low-plus,low-minus,mid-plus,mid-minus))

###########################################################################

data %>% 
  group_by(herbivores,height) %>% 
  summarise(count = n())
# we have 16 per group

###########################################################################

# we look at the distribution of the response variable Area_cm2
ggplot(data,aes(Area_cm2)) + geom_histogram(bins=30)
# doesnt look normal but the residuals need to be normal not the response.

# now we look at the distribution of Area_cm2 for the four treatment combinations
ggplot(data,aes(Area_cm2,color=herbivores,fill=height)) +
  geom_histogram(bins=30) +
  facet_wrap(~herbivores+height)
# some distribution still doesnt look so nice...

###########################################################################

# we have to square roo transform the Area_cm2
data <- data %>% 
  mutate(sqrt_Area = sqrt(Area_cm2))

###########################################################################

# graph effects of herbi and height, does effect of herbi differs 
# depending on height?

ggplot(data,aes(height,sqrt_Area,color=herbivores)) + 
  geom_boxplot() +
  geom_point(data=means, aes(height,mean,color=herbivores),shape=17,size=4)

###########################################################################
options(pillar.sigfig = 2)
# calculate means and sd for all 4 treatment conditions
means <- data %>% 
  group_by(herbivores,height) %>% 
  summarise(mean = mean(sqrt_Area),sd = sd(sqrt_Area))
means

###########################################################################

# fit the lm (i think there is an interaction)
model <- lm(sqrt_Area ~ herbivores * height,data)
autoplot(model)
# doesnt look bad...

# getting anova table
anova(model)
confint(model)
summary(model)

###########################################################################

# NICE GRAPH

ggplot(data=data, aes(height, sqrt_Area, color = herbivores)) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1,
                                           dodge.width = 1)) +
  geom_point(data=means, mapping=aes(y=mean), size=5, shape=3,
             position = position_dodge(width=1)) +
  labs(y=TeX('Algal cover - square root $cm^2$'), x = 'Height on the shoreline')

###########################################################################
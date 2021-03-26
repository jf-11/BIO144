########################################################################

# BIO144

########################################################################
# WEEK 5 IC EXERCISE 2
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
  bg = "white", fg = "black", accent = "#0CE3AC", 
  qualitative = RColorBrewer::brewer.pal(8,'Set1'),
  font = font_spec("serif", scale = 1.25)
)

########################################################################

# QUESTION:
# Answer the question of whether class reaction times show any evidence 
# of depending on the MASS OF THE INDIVIDUAL, and if this relationship 
# DEPENDS ON SEX. I.e. is there an interaction between sex and mass?

########################################################################

# load the dataset
data <- read.csv('http://docs.google.com/spreadsheets/d/e/2PACX-1vQFgYX1QhF9-UXep22XmPow1ZK5nbFHix9nkQIa0DzqUhPtZRxH1HtY-hsno32zDiuIHiLb2Hvphk1L/pub?gid=1188775314&single=true&output=csv')

########################################################################

# take a look at the data
View(data)

########################################################################

# modify the data that it is easier to work with

modified_data <- data %>% 
  rename(sex = What.was.your.biological.sex.at.birth., 
         reaction_time = Using.your.PREFERRED.hand.to.take.the.test..Please.enter.your.AVERAGE.reaction.time.in.milliseconds..e.g...326..,
         weight = Please.enter.your.weight..in.kilograms.)
final_data <- cbind(sex=modified_data$sex,weight=modified_data$weight,reaction_time=modified_data$reaction_time)

View(final_data)
str(final_data)

########################################################################

# making first plots

ggplot(final_data,aes(weight)) + geom_histogram()
ggplot(final_data,aes(reaction_time)) + geom_histogram()

# maybe log transform weight
final_data <- as.data.frame(final_data) %>% 
  mutate(log10_weight = log10(weight))
ggplot(final_data,aes(log10_weight)) + geom_histogram()
# looks better

ggplot(final_data,aes(log10_weight,reaction_time, color = sex)) +
  geom_point()

# weight doesnt seem to have a relationship with the reaction time...

########################################################################

# we fit the first model (reaction time and mass)
m1 <- lm(reaction_time ~ log10_weight,final_data)
autoplot(m1)
# normal plot looks ok, residuals vs fitted seems to be to close together
summary(m1)
# really low r squared..., very high p value for the slope...
# make not much sense to use this model...

########################################################################

# second model (reaction time weigh and sex)

m2 <- lm(reaction_time ~ weight * sex, final_data)
autoplot(m2)
summary(m2)

ggplot(final_data,aes(weight,reaction_time,color=sex)) +
  geom_point() +
  geom_smooth(method = 'lm')

ggplot(final_data,aes(as.factor(sex),weight)) + geom_boxplot()

########################################################################
########################################################################
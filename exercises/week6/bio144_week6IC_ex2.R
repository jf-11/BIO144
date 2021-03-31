########################################################################

# BIO144

###########################################################################
# WEEK 6 IC EXERCISE 2
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
# are men or women more beidhändig or is there no difference?

###########################################################################

# import data
class_RTs <- read.csv('http://docs.google.com/spreadsheets/d/e/2PACX-1vQFgYX1QhF9-UXep22XmPow1ZK5nbFHix9nkQIa0DzqUhPtZRxH1HtY-hsno32zDiuIHiLb2Hvphk1L/pub?gid=1188775314&single=true&output=csv')
class_RTs <- class_RTs %>% 
  rename(ID = Please.enter.the.unique.ID.code.you.gave.yourself.,
         Gender = What.was.your.biological.sex.at.birth.,
         Pref_Reaction_time = Using.your.PREFERRED.hand.to.take.the.test..Please.enter.your.AVERAGE.reaction.time.in.milliseconds..e.g...326..,
         Nonpref_Reaction_time_ave = Using.your.NON.PREFERRED.hand.to.take.the.test..Please.enter.your.average.reaction.time.in.milliseconds..e.g...326..)
class_RTs <- select(class_RTs, ID, Gender, Pref_Reaction_time, Nonpref_Reaction_time_ave)
dd <- tidyr::gather(class_RTs, key=key, value=Reaction_time, 3:4)
dd <- tidyr::separate(dd, col=key, sep="_", into=c("Hand", "Junk"))
dd <- select(dd, -Junk)

###########################################################################

# how can we analyze this?
# we can test if the difference is the same for weak hand to strong hand
# in males and females
# first we try to plot the data

ggplot(dd,aes(Reaction_time,color=Gender,fill=Hand)) +
  geom_histogram() +
  facet_wrap(~Hand+Gender)

ggplot(dd, aes(x=Gender, y=Reaction_time, col=Hand)) +
  geom_point(position = position_dodge(width=0.1))

ggplot(dd, aes(x=Reaction_time, col=Hand)) +
  geom_density() + facet_wrap(~Gender)

###########################################################################

# fitting the model
mod <- lm(Reaction_time ~ Gender * Hand,dd)
autoplot(mod)
anova(mod)
summary(mod)

# sex makes a difference but hand seems not to be important...

dd %>% 
  group_by(Hand,Gender) %>% 
  summarise(mean = mean(Reaction_time))

###########################################################################
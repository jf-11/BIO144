###########################################################################
###########################################################################
###########################################################################
###########################################################################

# BIO144

###########################################################################
# UNIT 12 IC EXERCISE 1
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
library(janitor)
library(lme4)
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
# Visually explore the data. Is variation within individuals large 
#(or not) compared to variation among individuals?
#Before doing the next step, make a table with the columns: Response 
#variable, fixed effect, random effect, R model formulae, r function used 
#to make model, effect size (i.e. difference between female and male reaction 
#times) and 95% confidence interval of this effect size, number of observations, 
#number of degrees of freedom for error.

###########################################################################
#Workflow:
##1. Label your skript (title, name, date)
##2. Come up with your question and write down the null-hypothesis.
##3. Import your data
##4. Check wether your data has been imported correctly.
##5. Plot the data.
##6. Plot the data for a statistical model.
##7. Check the assumptions of that model par(mfrow), plot(m1).
##8. Look at the summary.
##9. Make a nice graph for communication.
##10. Write down method and result section.
###########################################################################

# DATA 
data <- read.csv("~/Desktop/My reaction time (2021) - Form responses 1 (1).csv")

###########################################################################

# tidying data
# chaning names
names(data) <- c("Timestamp", "ID", "Gender", "Pref_Reaction_time_1",
                      "Verbal_memory_score", "Number_memory_score",
                      "Visual_memory_score",
                      "Weight_kgs", "Handed", "Nonpref_Reactiontime",  
                      "Pref_Reaction_time_2", "Pref_Reaction_time_3",  
                      "Pref_Reaction_time_4", "Pref_Reaction_time_5",
                      "Pref_Reactiontime", "Random_number")

# kick out unneccesary vars
data <- dplyr::select(data, -c('Timestamp',
                        'Verbal_memory_score',
                        'Number_memory_score',
                        'Visual_memory_score',
                        'Weight_kgs',
                        'Handed'))

data_new <- gather(data,
                      key = 'measurement',
                      value = 'react_time',
                      c('Pref_Reactiontime',  #key stands for the columns that are now put into one, later you can group them and use a legend
                        'Pref_Reaction_time_2',
                        'Pref_Reaction_time_3',
                        'Pref_Reaction_time_4',
                        'Pref_Reaction_time_5')) #value are the values of the columns that are no longer divided into several columns.
# my way of doing it...
dd <- data %>% 
  pivot_longer(cols = c('Pref_Reactiontime','Pref_Reaction_time_2',
                        'Pref_Reaction_time_3','Pref_Reaction_time_4',
                        'Pref_Reaction_time_5'),
               names_to = 'measurement', values_to = 'react_time')

###########################################################################

# Visually explore the data. Is variation within individuals large 
#(or not) compared to variation among individuals?

ggplot(dd, aes(x = ID, y = react_time))+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(aes(colour = measurement)) + 
  c_theme_sky

# there is both, but it seems like the variation within the individuals
# is larger. There is not much variation going on among the individuals.
# Withing individuals (boxes) there is some variation...

###########################################################################

# Response variable, fixed effect, random effect, R model formulae, 
# r function used to make model, effect size (i.e. difference between female and male reaction times) 
# and 95% confidence interval of this effect size, number of observations, 
# number of degrees of freedom for error

###########################################################################

# 1. Calculate and average the reaction time for each individual, and use these as the response variable.
# RESPONSE VARIABLE : mean of reaction time
# FIXED EFFECTS     : measurement
# RANDOM EFFECT     : ID 
# R MODEL           : mean_react_time ~
# R FUNCTION        : lm()
# 95% CI            : -45.61434 -14.0322
# EFFECT SIZE       : different slopes between gender
# OBSERVATIONS      : 200
# DF                : 198

# calculating average
mean_react_time <- dd %>% 
  group_by(ID,Gender) %>% 
  summarise(mean = mean(react_time))
arranged_data <- as.data.frame(mean_react_time)

linmod1 <- lm(mean~Gender,arranged_data)
summary(linmod1)
anova(linmod1)
ggPredict(linmod1) + c_theme_sky
autoplot(linmod1) + c_theme_sky

# the intercept states that there is a signigficant relationship between
# gender and the average of the reaction time.

###########################################################################

# 2. Without averaging, and Ignoring that multiple observations coming from each individual.
# RESPONSE VARIABLE : react_time
# FIXED EFFECTS     : Gender
# RANDOM EFFECT     : ID
# R MODEL           : react_time~
# R FUNCTION        : lm()
# 95% CI            : -39.14206 -22.39638
# OBSERVATIONS      : 1010
# DF                : 1008

linmod2 <- lm(react_time~Gender,dd)
summary(linmod2)
anova(linmod2)
ggPredict(linmod2) + aes(color=Gender) + c_theme_sky
autoplot(linmod2) + c_theme_sky

# states a really high relationship between Gender and the reaction time.

###########################################################################

# 3. Without averaging, and accounting for multiple observations coming from each individual.
# RESPONSE VARIABLE : reaction_time
# FIXED EFFECTS     : Gender
# RANDOM EFFECT     : ID
# R MODEL           : react_time~
# R FUNCTION        : lmer()
# 95% CI            :
# OBSERVATIONS      : 1010
# DF                :

linmod3 <- lmer(react_time ~ Gender + (1|ID), data=dd, REML = FALSE)
summary(linmod3)
anova(linmod3)
confint(linmod3)

# get p-value
#pf(1F,df1,df2,lower.tail=FALSE)

# this method does state a significant relationship between Gender
# and the reaction time by take multiple measurements into account.
# because 0 is not in the CI.

###########################################################################
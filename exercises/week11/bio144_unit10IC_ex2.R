###########################################################################
###########################################################################
###########################################################################
###########################################################################

# BIO144

###########################################################################
# UNIT 10 IC EXERCISE 2
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
library(pcsl)
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
###########################################################################
###########################################################################
###########################################################################

# DATA 
## Now read in the data, using the read_csv() function. We give it the URL of the published version of the google sheet data.
the_URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQFgYX1QhF9-UXep22XmPow1ZK5nbFHix9nkQIa0DzqUhPtZRxH1HtY-hsno32zDiuIHiLb2Hvphk1L/pub?gid=1188775314&single=true&output=csv"
data <- read_csv(the_URL)

## Must be very careful to get the next line right!!! Really important!!!
names(data) <- c("Timestamp", "ID", "Gender", "Pref_Reaction_time_1",
                      "Verbal_memory_score", "Number_memory_score",
                      "Visual_memory_score",
                      "Weight_kgs", "Handed", "Nonpref_Reactiontime",  
                      "Pref_Reaction_time_2", "Pref_Reaction_time_3",  
                      "Pref_Reaction_time_4", "Pref_Reaction_time_5",
                      "Pref_Reactiontime", "Random_number")
head(data)

maindata <- data %>%
  dplyr::select(Gender,Verbal_memory_score,Visual_memory_score,Number_memory_score)

# tidy data further
tidydata <- maindata %>% 
  pivot_longer(cols = c(Verbal_memory_score,Visual_memory_score,Number_memory_score), 
               names_to = 'method', values_to = 'score')

###########################################################################

# Number memory, Verbal memory, and Visual memory
# scientific style report on gender difference in these three types of memory

###########################################################################

# Define your question(s) clearly and keep it / them simple.
# Be sure to do the basics / fundamental / necessary parts very well.
# There is no need to do anything fancy!
# If you have time, make your report and everything in it beautiful.

###########################################################################

# QUESTION
# Is there a gender difference in these three tests?

###########################################################################

ggplot(tidydata) + aes(x=Gender, y=score) + geom_boxplot(aes(color=Gender)) + facet_grid(~method) + c_theme_sky
ggplot(tidydata) + aes(score) + geom_histogram() + facet_grid(~method) + c_theme_sky

tidydata %>%
  ggplot(aes(x = Gender, y = score)) +
  geom_jitter(width = 0.1, height = 0,aes(color=Gender)) +
  facet_wrap( ~ method, scale = "free") + c_theme_sky

###########################################################################

# now we fit glms
number_data <- tidydata %>% 
  filter(method=='Number_memory_score')

number.glm <- glm(score ~ Gender, data=number_data, family = poisson(link='log'))
autoplot(number.glm)
summary(number.glm)
number.glm.quasi <- glm(score ~ Gender, data=number_data, family = quasipoisson)
autoplot(number.glm.quasi)
summary(number.glm.quasi)
anova(number.glm.quasi,test = 'Chisq')

###########################################################################
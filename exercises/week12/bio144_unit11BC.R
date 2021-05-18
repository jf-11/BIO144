###########################################################################

# BIO144 BC UNIT 11

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

# INSECT DEATH VIDEO 1
# response can take two values --> 0 or 1...

# effect of pesticides on insect death
# different concentrations --> look how many died...
# relationships can look different, strong or little dose effect...
# if the differ --> there is an interaction

# data
data <- read.csv("~/Desktop/BIO144 Übungen/datasets_master/new_pesticide_study.csv")
head(data)
str(data)

# thinking about how to express the results

# success --> died, failure --> survived
data <- mutate(data, successes=dead,failures=n-dead)
head(data)

# proportion
data <- mutate(data, proportion_success=successes/n)
head(data)

# visualize the data
ggplot(data,aes(logdose,proportion_success,color=product)) +
  geom_point() + c_theme_sky
# the higher the dose, the greater the proportion of success...
# 100% and high doses, maybe red and green points under blue one on top right.
# do any of products look like they have a different slope? 
# --> maybe green one? already higher earlier...

# doing it wrong with linear model
m1 <- lm(proportion_success ~logdose * product,data = data)
# how many df? 63 - 6 parameters (3 slopes, 3 intercepts)= 57 df
autoplot(m1) + c_theme_sky
# big pattern in first plot, normal qq is ok, scale location also ok, leverage also...
# we see this pattern also in the data

# add regression line
ggplot(data,aes(logdose,proportion_success,color=product)) +
  geom_point() + c_theme_sky + geom_smooth(method='lm')
ggPredict(m1) + c_theme_sky # my clever option
# does not fit data very well and can predict unreal values

# summary table
summary(m1)
# little evidence
anova(m1)
# no statistically significance difference in slopes

###########################################################################

# INSECT DEATH VIDEO 2
# now we use glm with binomial family bc the data is binomial in nature
# data points were hidden behind others --> we have to jitter and mention it
# BINOMIAL RECAP --> tossing coin 10 times
rbinom(10,10,0.5)
dbinom(50,100,0.5) # probab of 50 heads when tossing 100 times

# logit of 1 --> infinity
# logit of 0 --> - infinity
# bounded to unbounded. response is bounded (0 to 1 bc probability)
# linear predictor scale is not bounded --> - to + infinity
logit(0)
logit(0.5)
logit(1)

# fit the glm
m2 <- glm(cbind(successes,failures) ~ logdose * product, 
          data = data, 
          family = binomial)
autoplot(m2) + c_theme_sky

# check wether interaction is significant or not
anova(m2, test = 'Chisq') # look at deviance table
# product explains very little deviance
# interaction --> 9 less explained deviance

summary(m2)
# green has a shallower slope --> correctly interpreted that

# look if overdispersed:
# --> no problem here resid dev close to df

###########################################################################

# VIDEO 3
# plot a binomial model
new_data <- expand.grid(logdose=seq(min(data$logdose), max(data$logdose),
                                    length=100),
                        product= unique(data$product))
p1 <- predict(m2,newdata = new_data, se.fit = TRUE, type='response')
n1 <- cbind(new_data,p1)
# plot 
ggplot(data,aes(logdose,proportion_success,color=product)) + c_theme_sky + 
  geom_smooth(data = n1, alpha=0.2,aes(logdose,fit, ymin=fit-se.fit,ymax=fit+se.fit, fill=product), stat = 'identity') +
  geom_point(size=2)

###########################################################################

# 5.2 Chi^2 Getting Started with R
# chisq.test()

###########################################################################
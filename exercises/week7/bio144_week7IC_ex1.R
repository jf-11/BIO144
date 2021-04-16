########################################################################

# BIO144

###########################################################################
# WEEK 7 IC EXERCISE 1
###########################################################################

rm(list=ls())
library(tidyverse)
library(ggthemes)
library(latex2exp)
library(ggfortify)
library(GGally)
library(gridExtra)

c_theme_bisq <- theme(legend.position="right",
                      text=element_text(size=12,
                                        family="serif")) +
  theme(panel.background = element_rect(fill = "bisque1")) +
  theme(axis.line = element_line(colour = "black"))

c_theme_withe <- theme(legend.position="right",
                      text=element_text(size=12,
                                        family="serif")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.line = element_line(colour = "black"))

c_theme_sky <- theme(legend.position="right",
                      text=element_text(size=12,
                                        family="serif")) +
  theme(panel.background = element_rect(fill = "lightskyblue")) +
  theme(axis.line = element_line(colour = "black"))

###########################################################################

# WORKFLOW --> PLOT --> MODEL --> CHECK ASSUMPTIONS --> INTERPRET --> PLOT

###########################################################################

# AFRICA IS SPECIAL

###########################################################################
###########################################################################
# data
data <- read.csv("~/Desktop/BIO144 Übungen/datasets_master/rugged.csv")
###########################################################################
# rugged --> this is the Terrain Ruggedness Index, that quantified the topographic 
# heterogeneity of the landscape. E.g., Netherlands = low, Switzerland = high.

# rgdppc_2000 --> this is real gross domestic product per capita in year 2000. 
# In his book McElreath puts forward a good argument for why one can already 
# expect such a variable to be exponentially distributed, such that we will 
# need to log transform it. His argument is based on growth being a multiplicative process.

# cont_africa --> this is a binary variable (0s and 1s) with 0 if a country is not 
# in Africa and 1 if it is.
###########################################################################
# Look at the distribution of the rgdppc_2000 variable -- and then create a 
# log10 transformed version of it.
ggplot(data) + 
  aes(rgdppc_2000) +
  geom_histogram(bins=30) +
  c_theme_bisq
# looks indeed exponentially distributed...
# now we transform it:
data <- data %>% 
  mutate(rgdppc_2000_log = log10(rgdppc_2000))
# take another look:
ggplot(data) + 
  aes(rgdppc_2000_log) +
  geom_histogram(bins=30) +
  c_theme_bisq
# looks better...
###########################################################################
# Create a new variable (name it cont_africa1) in which a row contains "Africa" 
# if the country is in Africa, and "not Africa" if it isn't. We do this, as its 
# easier and nicer to work with this information in the variable, rather than 
# just 0 and 1s. In general, in R, don't use codes for things that can be 
# represented by words. Use the meaningful words. This is a good thing to do, 
# because then tables and graphs have those words, rather than cryptic codes, 
# and will be easier for you and others to interpret. Hint: use the function 
# ifelse inside mutate.
data <- data %>% 
  mutate(cont_africa1 = ifelse(cont_africa==1,'Africa','not Africa'))
head(data$cont_africa1)
# seems like it worked
###########################################################################
# how many countries in the dataset are in Africa?
table(data$cont_africa1)
# --> 57 in Africa
# --> 177 not in Africa
###########################################################################
# What does the distribution of the variable "rugged" that describes landscape 
# ruggedness look like?
ggplot(data) + 
  aes(rugged) + 
  geom_histogram(bins=30) +
  c_theme_bisq
# looks really skewed
###########################################################################
# Which country has the lowest ruggedness, i.e. which is the flattest country?
data$country[which.min(data$rugged)]
# --> Tokelau
###########################################################################
# In the league table of ruggedness, where does Switzerland come?
rugged_ranking <- data[,c(3,4)]
rugged_ranking <- rugged_ranking[order(rugged_ranking$rugged,decreasing=TRUE),]
rugged_ranking <- cbind(rugged_ranking,rank =1:nrow(rugged_ranking))
rugged_ranking[rugged_ranking$country=='Switzerland',]
# --> 10
###########################################################################
# How many NA values are there in the the gdp variable?
sum(is.na(data$rgdppc_2000))
# How many of theese are countries in Africa?
nrow(data[data$cont_africa==1 & is.na(data$rgdppc_2000),])
# How many of these are countries not in Africa?
nrow(data[data$cont_africa==0 & is.na(data$rgdppc_2000),])
###########################################################################
# Now plot a graph that will tell you the answer to the question... does the 
# relationship between gdp and ruggedness differ between countries in Africa 
# and those not in Africa.
# Make a guess of if you think there is a statistically significant difference.
ggplot(data) +
  aes(rugged,rgdppc_2000_log, color = cont_africa1) +
  geom_point() +
  c_theme_bisq
# i cant see anything...
ggplot(data, aes(x=rugged, y=rgdppc_2000_log, colour=cont_africa1)) +
  geom_point() +
  c_theme_bisq
## not totally clear eh!
## try with different facets for africa and not africa
ggplot(data, aes(x=rugged, y=rgdppc_2000_log)) +
  geom_point() +
  facet_wrap(~cont_africa1) +
  c_theme_bisq
###########################################################################
# How many degrees of freedom will your statistical model have? 
#( To answer this, you have to figure out what your model should be, in order 
# to address the question we're trying to answer with the data.)
lin.mod <- lm(rgdppc_2000_log ~ rugged * cont_africa1, data)
# Your model will have four parameters, there are 170 countries with data, so 166
# two intercepts, two slopes
# na countries cant be used
###########################################################################
# How does the QQ-plot of your model make you feel?
autoplot(lin.mod) + c_theme_bisq
# not totally happy
###########################################################################
# How does the distribution of the residuals look?
resids <- as.data.frame(residuals(lin.mod))
ggplot(resids) + aes(residuals(lin.mod)) + geom_histogram(bins = 12) + 
  c_theme_bisq
# looks quite bimodal
###########################################################################
# is africa special?
anova(lin.mod)
# interaction term is quite significant
###########################################################################
# Rounded to two significant figures, what is the slope of the relationship 
# (between ruggedness and gdp) for countries in Africa?
summary(lin.mod)
# 0.083
# for countries not in africa?
0.08275 - 0.17085
# -0.088
###########################################################################
# explanatory power
# 0.3453
# Obviously there are a lot of other things affecting a country's gdp
###########################################################################
# Now make a beautiful graph with the fitted lines and confidence intervals 
# of the lines.
# we have to use expand.grid and predict
# lm(rgdppc_2000_log ~ rugged * cont_africa1, data)

new_data <- expand.grid(
  rugged=seq(from=min(data$rugged), to=max(data$rugged), length.out = 100),
  cont_africa1= unique(data$cont_africa1))
head(new_data)

rgdppc_2000_log <- predict(lin.mod, newdata = new_data, interval = 'confidence')
head(rgdppc_2000_log)

plotting_data <- data.frame(new_data, rgdppc_2000_log)
head(plotting_data)
plotting_data <- rename(plotting_data,rgdppc_2000_log=fit)
head(plotting_data)

# FINAL PLOT
ggplot(data, aes(rugged, rgdppc_2000_log)) +
  geom_point() +
  facet_wrap(~cont_africa1) +
  geom_smooth(data=plotting_data,
              aes(x=rugged,y=rgdppc_2000_log,ymin=lwr, ymax=upr), 
              stat = 'identity') +
  c_theme_bisq
###########################################################################
# seperate regressions
m2 <- lm(rgdppc_2000_log ~ rugged, filter(data, cont_africa1=="Africa"))
m3 <- lm(rgdppc_2000_log ~ rugged, filter(data, cont_africa1=="not Africa"))

new_data2 <- expand.grid(rugged=seq(min(data$rugged), max(data$rugged), length=100))
p2 <- predict(m2, newdata = new_data2, interval="confidence")
n2 <- cbind(new_data2, p2, cont_africa1="Africa")
p3 <- predict(m3, newdata = new_data2, interval="confidence")
n3 <- cbind(new_data2, p3, cont_africa1="not Africa")
n4 <- rbind(n2, n3)
ggplot(data, aes(x=rugged, y=rgdppc_2000_log)) +
  geom_point() +
  facet_wrap(~cont_africa1) +
  geom_smooth(data=n4, aes(y=fit, ymin=lwr, ymax=upr), stat="identity") +
  xlab("Terrain Ruggedness Index") +
  ylab("Log10 Real Gross Domestic Product per capita,\nfrom year2000") +
  c_theme_bisq
###########################################################################
## What if we did the linear model on the data with the NAs
m2 <- lm(rgdppc_2000_log ~ rugged * cont_africa1, data)
## no warning!!!
summary(m2)
## Correct number of degrees of freedom, so lm has removed the NAs
## Better to do this ourselves, rather than let lm do it with no notification.
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
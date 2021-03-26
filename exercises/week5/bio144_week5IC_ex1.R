########################################################################

# BIO144

########################################################################
# WEEK 5 IC EXERCISE 1
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

# QUESTION
# The dataset you'll look at is about naked mole rats, and what influences how much energy the expend
# Naked mole rats are particularly interesting animals, because they are the only known mammals
# with distinct social castes (well, apart from humans perhaps). Within the worker caste there
# seem to be two castes: hard workers, and lazy individuals. Researchers were interested
# in physiological difference between the hard workers and the lazy individuals, so 
# measured their energy expenditure, and their body mass. Mass was measured because
# it was already known that the lazy individuals are bigger than the hard workers.
# So, the question was "after taking into account differences in mass, was there a difference
# in energy expenditure?".

########################################################################

# first we load the data
dd <- read.csv("~/Desktop/BIO144 Übungen/datasets_master/MoleRatLayabouts.csv")

########################################################################

# how many workers or how many are lazy?
table(dd$caste)

########################################################################

# looking at distributions of Mass and eneregy
p1 <- ggplot(dd,aes(Mass)) + geom_histogram(bins=20)
p2 <- ggplot(dd,aes(Energy)) + geom_histogram(bins=20)
grid.arrange(p1,p2)
# both look a bit left skewed

########################################################################

# we log10 transform both vairables because they both look skewed...
dd <- dd %>% 
  mutate(log10_mass = log10(Mass),log10_energy = log10(Energy))
p1 <- ggplot(dd,aes(log10_mass)) + geom_histogram(bins=20)
p2 <- ggplot(dd,aes(log10_energy)) + geom_histogram(bins=20)
grid.arrange(p1,p2)
# looks much better

########################################################################

# now we make a plot to analyse the biological question
ggplot(dd, aes(log10_mass,log10_energy, color = caste)) +
  geom_point()
# we can see that lazy individuals are usually higher in mass. for a 
# given mass worker seems to have higher energy expenditure.

########################################################################

# theoretical
# regression lines for both castes --> would they differ in their slope?
# --> I think so, seems that workers would have a higher slope...

########################################################################

# which model?

# A: lm(log10_Energy ~ caste, dd) --> if energy use differs between the castes, irrespective of mass (2params)

# B: lm(log10_Energy ~ log10_Mass, dd) -->  overall relationship between energy and mass, ignoring caste (2params)

# C: lm(log10_Energy ~ caste + log10_Mass, dd) --> two main effects and no interaction terms (3params)

# D: lm(log10_Energy ~ caste * log10_Mass, dd) --> show if the slopes of the energy-mass relationship differ between the two castes (4params)

########################################################################

# model D
# how many df in model D?
nrow(dd) - 4

# fitting the model
md <- lm(log10_energy ~ caste * log10_mass, dd)
# check the assumptions
autoplot(md)

# whats the estimated difference in slope between castes?
summary(md)
# lazy is reference --> 0.42 difference
# standard error --> 0.41
# high p-value too so difference in slope is not stat significant

########################################################################

# fitting model C
mc <- lm(log10_energy ~ caste + log10_mass, dd)
# checking assumptions
autoplot(mc)
# looks fine
# difference in R^2 with model D -->  
0.4278 - 0.409

########################################################################

# PRODUCING FIGURE THAT SHOWS BEST SUPPORTED STATISTICAL MODEL
# the modle without the interaction term was the best, so we will plot 
# tis one...

new_data <- expand.grid(log10_mass=seq(min(dd$log10_mass), max(dd$log10_mass), length=100),
                        caste=unique(dd$caste))
## make predictions with that data and model m2 (no interaction)
p1 <- predict(mc, newdata = new_data, interval = "confidence")
## combine the predicted and new data to make plotting easier
n1 <- cbind(p1, new_data)
## And make the graph
ggplot(dd, aes(x=log10_mass, y=log10_energy, colour=caste)) +
  geom_point() +
  geom_smooth(data=n1, mapping=aes(y=fit, ymin=lwr, ymax=upr), stat="identity") +
  labs(x=TeX('$log_{10} Mass$'),y=TeX('$log_{10} Energy$'),title='Naked Mole Rats')
# predict the values for 100 mass numbers and making confidence interval and
# plotting it with geom_smooth, geom_smooth inherits the color of caste
########################################################################
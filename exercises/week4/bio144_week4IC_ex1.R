########################################################################

# BIO144

########################################################################
# WEEK 4 IC EXERCISE 1
########################################################################

rm(list=ls())
library(tidyverse)
library(ggthemes)
library(latex2exp)
library(ggfortify)
library(GGally)
library(gridExtra)

########################################################################

# FUN WITH QQ PLOTS
# NORMALLY DISTRIBUTED RESIDUALS

# HISTOGRAM
resids <- rnorm(100, 0, 1)
ggplot(as.data.frame(resids)) + aes(resids) + geom_histogram(aes(y=..density..),bins=30) +
  stat_function(fun = dnorm,aes(color='red')) +
  scale_colour_manual(name='Legend',values = 'red',breaks = 'red',labels='normal dist')

# QQPLOT
qqnorm(resids)
qqline(resids,col='red')

ggplot(as.data.frame(resids), aes(sample=resids)) + stat_qq() + stat_qq_line(aes(color='red')) +
  theme(legend.position = "none")

########################################################################

# SOME DISTRIBUTIONS AND THEIR QQ-PLOTS

par(mfrow=c(4,4))

hist(rnorm(1000,0,1))
qqnorm(rnorm(1000,0,1))
qqline(rnorm(1000,0,1),col='red')

hist(runif(1000,0,1))
qqnorm(runif(1000,0,1))
qqline(runif(1000,0,1),col='red')

hist(rbeta(1000,5,2))
qqnorm(rbeta(1000,5,2))
qqline(rbeta(1000,5,2),col='red')

hist(rbeta(1000,2,5))
qqnorm(rbeta(1000,2,5))
qqline(rbeta(1000,2,5),col='red')

hist(rlnorm(1000,0,1))
qqnorm(rlnorm(1000,0,1))
qqline(rlnorm(1000,0,1),col='red')

hist(rpois(1000,1))
qqnorm(rpois(1000,1))
qqline(rpois(1000,1),col='red')

hist(rbinom(10,1000,0.2))
qqnorm(rbinom(10,1000,0.2))
qqline(rbinom(10,1000,0.2),col='red')

########################################################################

# BACK TO HEALTH CARE SPENDING DIAGNOSTICS

financinghealthcare <- read.csv("~/Desktop/BIO144 Übungen/datasets_master/financing_healthcare.csv")
reduced_data <- financinghealthcare %>% 
  filter(!is.na(health_exp_total) & !is.na(child_mort) & year == 2013)
transformed_data <- reduced_data %>% 
  mutate(log_health = log10(health_exp_total), log_child = log10(child_mort))
lin_mod <- lm(log_child~log_health,data=transformed_data)

# DIAGNOSTIC PLOTS
autoplot(lin_mod,which = 1:2,smooth.colour = NA)
# points with numbers are potential outliers

########################################################################







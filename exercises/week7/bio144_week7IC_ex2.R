########################################################################

# BIO144

###########################################################################
# WEEK 7 IC EXERCISE 2
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
# LINEAR ALGEBRA
###########################################################################
# Repeat the example given in slides 40-43 of lecture 7 using R.
x1 <- c(0,1,2,3,4)
x2 <- c(4,1,0,1,4)
Xtilde <- matrix(c(rep(1,5),x1,x2),ncol=3)
Xtilde

t.beta <- c(10,5,-2)
t.y <- Xtilde%*%t.beta
t.y

t.e <-rnorm(5,0,1)
t.e

t.Y <- t.y + t.e
t.Y

r.lm <- lm(t.Y ~ x1 + x2)

solve(t(Xtilde) %*% Xtilde) %*% t(Xtilde) %*% t.Y
###########################################################################
t.E <- matrix(rnorm(500),ncol=100)
t.Y <- matrix(rep(t.y,100),nrow=5,byrow=F)
y.Y <- t.Y+ t.E
y.Y
r.coef <- matrix(NA,ncol=3, nrow=100)
for (i in 1:100) {
  r.coef[i,] <- lm(y.Y[,i] ~ x1 + x2)$coefficients
}
data <- as.data.frame(r.coef)
data <- cbind(data, simulations = seq(1:nrow(data)))
head(data)
data <- rename(data, 'b0'=V1,'b1'=V2,'b2'=V3)
head(data)
p <- ggplot(data) + aes(b1,b0) + geom_point() + c_theme_withe
pp <- ggplot(data) + aes(b2,b0) + geom_point() + c_theme_withe
ppp <- ggplot(data) + aes(b2,b1) + geom_point() + c_theme_withe
grid.arrange(p,pp,ppp)
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################